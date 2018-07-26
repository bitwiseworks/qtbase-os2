/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** Copyright (C) 2018 bww bitwise works GmbH. OS/2 parts.
**
** This file is part of the QtCore module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 3 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL3 included in the
** packaging of this file. Please review the following information to
** ensure the GNU Lesser General Public License version 3 requirements
** will be met: https://www.gnu.org/licenses/lgpl-3.0.html.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 2.0 or (at your option) the GNU General
** Public license version 3 or any later version approved by the KDE Free
** Qt Foundation. The licenses are as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL2 and LICENSE.GPL3
** included in the packaging of this file. Please review the following
** information to ensure the GNU General Public License requirements will
** be met: https://www.gnu.org/licenses/gpl-2.0.html and
** https://www.gnu.org/licenses/gpl-3.0.html.
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "qplatformdefs.h"
#include "qt_os2.h"

#include "qeventdispatcher_os2_p.h"

#include "qcoreapplication.h"
#include "qsocketnotifier.h"
#include "qmutex.h"
#include "qwaitcondition.h"
#include "qset.h"

#include "qabstracteventdispatcher_p.h"
#include "qcoreapplication_p.h"
#include "private/qcore_unix_p.h"
#include "private/qtimerinfo_unix_p.h"

#include "private/qthread_p.h"

QT_BEGIN_NAMESPACE

extern uint qGlobalPostedEventsCount();

ULONG WM_QT_TIMER_OR_SOCKET = 0;

// NOTE: Socket and timer handling is largely borrowed from qeventdispatcher_unix*.

static const char *socketType(QSocketNotifier::Type type)
{
    switch (type) {
    case QSocketNotifier::Read:
        return "Read";
    case QSocketNotifier::Write:
        return "Write";
    case QSocketNotifier::Exception:
        return "Exception";
    }

    Q_UNREACHABLE();
}

struct QSocketNotifierSetOS2 final
{
    inline QSocketNotifierSetOS2() Q_DECL_NOTHROW;

    inline bool isEmpty() const Q_DECL_NOTHROW;
    inline short events() const Q_DECL_NOTHROW;

    QSocketNotifier *notifiers[3];
};

inline QSocketNotifierSetOS2::QSocketNotifierSetOS2() Q_DECL_NOTHROW
{
    notifiers[0] = 0;
    notifiers[1] = 0;
    notifiers[2] = 0;
}

inline bool QSocketNotifierSetOS2::isEmpty() const Q_DECL_NOTHROW
{
    return !notifiers[0] && !notifiers[1] && !notifiers[2];
}

inline short QSocketNotifierSetOS2::events() const Q_DECL_NOTHROW
{
    short result = 0;

    if (notifiers[0])
        result |= POLLIN;

    if (notifiers[1])
        result |= POLLOUT;

    if (notifiers[2])
        result |= POLLPRI;

    return result;
}


class QEventDispatcherOS2Private : public QAbstractEventDispatcherPrivate
{
    Q_DECLARE_PUBLIC(QEventDispatcherOS2)

public:
    QEventDispatcherOS2Private();
    ~QEventDispatcherOS2Private();

    void createMsgQueue();
    void destroyMsgQueue();

    int activateTimersAndSockets(bool activateSockets);

    void startThread();
    void stopThread();
    void notifyThread();

    void cancelSelectOrWait();

    static void threadMain(void *arg);

    HAB hab;
    HMQ hmq;

    QAtomicInt interrupt; // bool

    QList<QMSG> queuedUserInputEvents;
    QList<QMSG> queuedSocketEvents;

    int tid;
    QWaitCondition cond;

    mutable QMutex mutex; // guards access to further variables

    int unblockFd;

    QHash<int, QSocketNotifierSetOS2> socketNotifiers;
    QSet<QSocketNotifier *> pendingNotifiers;

    QTimerInfoList timerList;
};

QEventDispatcherOS2Private::QEventDispatcherOS2Private()
    : hab(NULLHANDLE), hmq(NULLHANDLE), interrupt(false), tid(0), unblockFd(-1)
{
    if (WM_QT_TIMER_OR_SOCKET == 0)
        WM_QT_TIMER_OR_SOCKET = WinAddAtom(WinQuerySystemAtomTable(), "QEventDispatcherOS2:WM_QT_TIMER_OR_SOCKET");
}

QEventDispatcherOS2Private::~QEventDispatcherOS2Private()
{
    Q_ASSERT(hmq == NULLHANDLE);
    Q_ASSERT(tid == 0);
}

void QEventDispatcherOS2Private::createMsgQueue()
{
    if (hmq == NULLHANDLE) {
        // first, dynamically switch ("morph") to PM mode if we have been
        // compiled as the console application. This is necessary to create the
        // event queue, windows and other PM resources. As a side effect, the
        // console remains attached after morphing which can be useful for
        // debugging
        PPIB ppib;
        DosGetInfoBlocks(NULL, &ppib);
        if (ppib->pib_ultype != 3)
            ppib->pib_ultype = 3;
        // then create the message queue
        hab = WinInitialize(0);
        hmq = WinCreateMsgQueue(hab, 0);
        if (hmq == NULLHANDLE)
            qWarning("QEventDispatcherOS2Private: WinCreateMsgQueue failed with 0x%08lX",
                     WinGetLastError(hab));
    }
}

void QEventDispatcherOS2Private::destroyMsgQueue()
{
    if (hmq != NULLHANDLE) {
        stopThread();
        WinDestroyMsgQueue(hmq);
        WinTerminate(hab);
    }
}

int QEventDispatcherOS2Private::activateTimersAndSockets(bool activateSockets)
{
    QMutexLocker locker(&mutex);

    int n_activated = 0;

    if (activateSockets && !pendingNotifiers.isEmpty()) {
        QEvent event(QEvent::SockAct);

        auto it = pendingNotifiers.begin();
        while (it != pendingNotifiers.end()) {
            QSocketNotifier *notifier = *it;
            QCoreApplication::sendEvent(notifier, &event);
            ++n_activated;
            it = pendingNotifiers.erase(it);
        }
    }

    n_activated += timerList.activateTimers();

    return n_activated;
}

void QEventDispatcherOS2Private::startThread()
{
    if (tid == 0) {
        tid = _beginthread(threadMain, nullptr, 0, this);
        if (tid == -1)
          qErrnoWarning(errno, "QEventDispatcherOS2Private: _beginthread failed");
    }
}

void QEventDispatcherOS2Private::stopThread()
{
    if (tid != 0) {
        QMutexLocker locker(&mutex);

        // remove all socket notifiers and timers to stop the AUX thread
        socketNotifiers.clear();
        timerList.clear();
        cancelSelectOrWait();

        locker.unlock();

        // wait for the thread to stop
        TID tid_os2 = tid;
        APIRET arc = DosWaitThread(&tid_os2, DCWW_WAIT);
        if (arc)
          qWarning("QEventDispatcherOS2Private: DosWaitThread failed with %ld", arc);

        tid = 0;
    }
}

// NOTE: Caller must hold the mutex.
void QEventDispatcherOS2Private::notifyThread()
{
    if (tid == 0)
        startThread();
    else
        cancelSelectOrWait();
}

// NOTE: Caller must hold the mutex.
void QEventDispatcherOS2Private::cancelSelectOrWait()
{
    if (unblockFd < 0) {
        // Terminate the idle or simple wait state.
        cond.wakeOne();
    } else {
        // Terminate select() execution.
        ::so_cancel(unblockFd);
    }

    // Indicate that this is a cancel request.
    unblockFd = -2;
}

// static
void QEventDispatcherOS2Private::threadMain(void *arg)
{
    QEventDispatcherOS2Private *d = reinterpret_cast<QEventDispatcherOS2Private *>(arg);

    static struct {
        QSocketNotifier::Type type;
        fd_set set;
    } sets[] = {
        { QSocketNotifier::Read, { 0 } },
        { QSocketNotifier::Write, { 0 } },
        { QSocketNotifier::Exception, { 0 } }
    };
    constexpr int sets_size = sizeof(sets) / sizeof(sets[0]);

    QMutexLocker locker(&d->mutex);

    forever {
        bool hasSockets = false;

        // Finish the thread if there is nothing to do.
        if (d->socketNotifiers.isEmpty() || d->timerList.isEmpty())
            break;

        // Get the maximum time we can wait.
        timeval *timeout = nullptr;
        timeval wait_tm = { 0l, 0l };
        timespec wait_tm_ts = { 0l, 0l };
        if (d->timerList.timerWait(wait_tm_ts)) {
            wait_tm = timespecToTimeval(wait_tm_ts);
            timeout = &wait_tm;
        }

        if (!d->socketNotifiers.isEmpty()) {
            for (int i = 0; i < sets_size; ++i)
                FD_ZERO(&sets[i].set);

            int maxfd = -1;

            for (auto it = d->socketNotifiers.cbegin(); it != d->socketNotifiers.cend(); ++it) {
                const QSocketNotifierSetOS2 &sn_set = it.value();
                for (int i = 0; i < sets_size; ++i) {
                    QSocketNotifier *sn = sn_set.notifiers[sets[i].type];
                    // NOTE: ignore sockets that are already pending to avoid polluting the
                    // PM message queue with socket messages if the target window is not fast
                    // enough to process them. This will also ensure that this thread won't be
                    // spinning in a tight loop because of immediate select returns.
                    if (sn && !d->pendingNotifiers.contains(sn)) {
                        int sockfd = sn->socket();
                        FD_SET(sockfd, &sets[i].set);
                        // memorize the highest fd
                        if (maxfd < sockfd)
                            maxfd = sockfd;
                    }
                }
            }

            Q_ASSERT(maxfd);

            // Set the socket fd used to cancel select
            d->unblockFd = maxfd;

            locker.unlock();

            int nsel = ::select(maxfd + 1, &sets[0].set, &sets[1].set, &sets[2].set, timeout);

            if (nsel == -1 && errno == EINVAL) {
                qWarning("QSocketNotifier: select() returned EINVAL, check that "
                         "the socket is not an OS/2 file handle.");
                // Give it some sleep to avoid 100% CPU load due to select()
                // constantly failing in a tight loop which freezes the system.
                DosSleep(100);
            }

            locker.relock();

            if (nsel > 0) {
                // Find out which sockets have been reported and mark them as pending.
                for (auto it = d->socketNotifiers.cbegin(); it != d->socketNotifiers.cend(); ++it) {
                    const QSocketNotifierSetOS2 &sn_set = it.value();
                    for (int i = 0; i < sets_size; ++i) {
                        QSocketNotifier *sn = sn_set.notifiers[sets[i].type];
                        if (sn && FD_ISSET(sn->socket(), &sets[i].set) && !d->pendingNotifiers.contains(sn)) {
                            d->pendingNotifiers << sn;
                            hasSockets = true;
                        }
                    }
                }
            }
        } else {
            unsigned long msecs = timeout ?
                timeout->tv_sec * 1000ul + timeout->tv_usec / 1000ul : ULONG_MAX;

            // Indicate that we're waiting.
            d->unblockFd = -1;

            d->cond.wait(locker.mutex(), msecs);
        }

        // Inform this dispatcher's event queue unless it's a cancel request.
        if (d->unblockFd != -2)
            WinPostQueueMsg(d->hmq, WM_QT_TIMER_OR_SOCKET, MPFROMLONG(hasSockets), NULL);
    }
}

QEventDispatcherOS2::QEventDispatcherOS2(QObject *parent)
    : QAbstractEventDispatcher(*new QEventDispatcherOS2Private, parent)
{
}

QEventDispatcherOS2::~QEventDispatcherOS2()
{
}

bool QEventDispatcherOS2::processEvents(QEventLoop::ProcessEventsFlags flags)
{
    Q_D(QEventDispatcherOS2);

    Q_ASSERT(d->hmq);

    d->interrupt.store(0);

    emit awake();

    bool canWait;
    bool retVal = false;

    QMSG waitMsg;
    bool haveWaitMsg = false;

    do {
        QCoreApplicationPrivate::sendPostedEvents(0, 0, d->threadData);

        while (!d->interrupt.load()) {
            QMSG msg;
            bool haveMessage;

            if (!(flags & QEventLoop::ExcludeUserInputEvents) && !d->queuedUserInputEvents.isEmpty()) {
                // Process queued user input events.
                haveMessage = true;
                msg = d->queuedUserInputEvents.takeFirst();
            } else if(!(flags & QEventLoop::ExcludeSocketNotifiers) && !d->queuedSocketEvents.isEmpty()) {
                // Process queued socket events.
                haveMessage = true;
                msg = d->queuedSocketEvents.takeFirst();
            } else {
                if (haveWaitMsg) {
                    haveMessage = true;
                    msg = waitMsg;
                    haveWaitMsg = false;
                } else {
                    haveMessage = WinPeekMsg(d->hab, &msg, 0, 0, 0, PM_REMOVE) == TRUE;
                }
                if (haveMessage && (flags & QEventLoop::ExcludeUserInputEvents) &&
                    (msg.msg == WM_CHAR ||
                     (msg.msg >= WM_MOUSEFIRST &&
                      msg.msg <= WM_MOUSELAST) ||
                     (msg.msg >= WM_EXTMOUSEFIRST &&
                      msg.msg <= WM_EXTMOUSELAST) ||
                     msg.msg == WM_HSCROLL ||
                     msg.msg == WM_VSCROLL)) {
                    // Queue user input events for later processing.
                    haveMessage = false;
                    d->queuedUserInputEvents.append(msg);
                }
                if (haveMessage && (flags & QEventLoop::ExcludeSocketNotifiers)
                    && (msg.msg == WM_QT_TIMER_OR_SOCKET && LONGFROMMP(msg.mp1))) {
                    // Queue socket events for later processing.
                    haveMessage = false;
                    d->queuedSocketEvents.append(msg);
                }
            }
            if (haveMessage) {
                if (msg.msg == WM_QUIT) {
                    if (QCoreApplication::instance()) {
                        QCoreApplication::instance()->quit();
                        return true;
                    }
                    return false;
                }

                if (!filterNativeEvent(QByteArrayLiteral("os2_generic_QMSG"), &msg, 0)) {
                    if (msg.msg == WM_QT_TIMER_OR_SOCKET) {
                        d->activateTimersAndSockets(LONGFROMMP(msg.mp1));
                    } else {
                        WinDispatchMsg(d->hab, &msg);
                    }
                }
            } else {
                // Nothing todo, so break.
                break;
            }
            retVal = true;
        }

        // Still nothing - wait for message.
        canWait = (!retVal
                   && (flags & QEventLoop::WaitForMoreEvents)
                   && d->threadData->canWaitLocked()
                   && !d->interrupt.load());
        if (canWait) {
            emit aboutToBlock();
            WinGetMsg(d->hab, &waitMsg, 0, 0, 0);
            haveWaitMsg = true;
            emit awake();
        }
    } while (canWait);

    return retVal;
}

bool QEventDispatcherOS2::hasPendingEvents()
{
    QMSG msg;
    return qGlobalPostedEventsCount() || WinPeekMsg(0, &msg, NULL, 0, 0, PM_NOREMOVE);
}

void QEventDispatcherOS2::registerSocketNotifier(QSocketNotifier *notifier)
{
    Q_ASSERT(notifier);
    int sockfd = notifier->socket();
    QSocketNotifier::Type type = notifier->type();
#ifndef QT_NO_DEBUG
    if (sockfd < 0 || unsigned(sockfd) >= FD_SETSIZE) {
        qWarning("QSocketNotifier: socket fd is not in [0, FD_SETSIZE]");
        return;
    } else if (notifier->thread() != thread() || thread() != QThread::currentThread()) {
        qWarning("QSocketNotifier: socket notifiers cannot be enabled from another thread");
        return;
    }
#endif

    Q_D(QEventDispatcherOS2);

    QMutexLocker locker(&d->mutex);

    QSocketNotifierSetOS2 &sn_set = d->socketNotifiers[sockfd];

    if (sn_set.notifiers[type] && sn_set.notifiers[type] != notifier)
        qWarning("%s: Multiple socket notifiers for same socket %d and type %s",
                 Q_FUNC_INFO, sockfd, socketType(type));

    sn_set.notifiers[type] = notifier;

    d->notifyThread();
}

void QEventDispatcherOS2::unregisterSocketNotifier(QSocketNotifier *notifier)
{
    Q_ASSERT(notifier);
    int sockfd = notifier->socket();
    QSocketNotifier::Type type = notifier->type();
#ifndef QT_NO_DEBUG
    if (sockfd < 0 || unsigned(sockfd) >= FD_SETSIZE) {
        qWarning("QSocketNotifier: socket fd is not in [0, FD_SETSIZE]");
        return;
    } else if (notifier->thread() != thread() || thread() != QThread::currentThread()) {
        qWarning("QSocketNotifier: socket notifier (fd %d) cannot be disabled from another thread.\n"
                 "(Notifier's thread is %s(%p), event dispatcher's thread is %s(%p), current thread is %s(%p))",
                 sockfd,
                 notifier->thread() ? notifier->thread()->metaObject()->className() : "QThread", notifier->thread(),
                 thread() ? thread()->metaObject()->className() : "QThread", thread(),
                 QThread::currentThread() ? QThread::currentThread()->metaObject()->className() : "QThread", QThread::currentThread());
        return;
    }
#endif

    Q_D(QEventDispatcherOS2);

    QMutexLocker locker(&d->mutex);

    d->pendingNotifiers.remove(notifier);

    auto i = d->socketNotifiers.find(sockfd);
    if (i == d->socketNotifiers.end())
        return;

    QSocketNotifierSetOS2 &sn_set = i.value();

    if (sn_set.notifiers[type] == nullptr)
        return;

    if (sn_set.notifiers[type] != notifier) {
        qWarning("%s: Multiple socket notifiers for same socket %d and type %s",
                 Q_FUNC_INFO, sockfd, socketType(type));
        return;
    }

    sn_set.notifiers[type] = nullptr;

    if (sn_set.isEmpty())
        d->socketNotifiers.erase(i);

    d->notifyThread();
}

void QEventDispatcherOS2::registerTimer(int timerId, int interval, Qt::TimerType timerType, QObject *object)
{
#ifndef QT_NO_DEBUG
    if (timerId < 1 || interval < 0 || !object) {
        qWarning("QEventDispatcherOS2::registerTimer: invalid arguments");
        return;
    } else if (object->thread() != thread() || thread() != QThread::currentThread()) {
        qWarning("QEventDispatcherOS2: timers cannot be started from another thread");
        return;
    }
#endif

    Q_D(QEventDispatcherOS2);

    QMutexLocker locker(&d->mutex);

    d->timerList.registerTimer(timerId, interval, timerType, object);

    d->notifyThread();
}

bool QEventDispatcherOS2::unregisterTimer(int timerId)
{
#ifndef QT_NO_DEBUG
    if (timerId < 1) {
        qWarning("QEventDispatcherOS2::unregisterTimer: invalid argument");
        return false;
    } else if (thread() != QThread::currentThread()) {
        qWarning("QEventDispatcherOS2: timers cannot be stopped from another thread");
        return false;
    }
#endif

    Q_D(QEventDispatcherOS2);

    QMutexLocker locker(&d->mutex);

    bool result = d->timerList.unregisterTimer(timerId);

    // Note that we only notify the AUX thread when there is nothing to wait
    // for, as otherwise it's harmless to wait for the next timer or socket.
    if (result && d->timerList.isEmpty() && d->socketNotifiers.isEmpty())
        d->notifyThread();

    return result;
}

bool QEventDispatcherOS2::unregisterTimers(QObject *object)
{
#ifndef QT_NO_DEBUG
    if (!object) {
        qWarning("QEventDispatcherOS2::unregisterTimers: invalid argument");
        return false;
    } else if (object->thread() != thread() || thread() != QThread::currentThread()) {
        qWarning("QEventDispatcherOS2: timers cannot be stopped from another thread");
        return false;
    }
#endif

    Q_D(QEventDispatcherOS2);

    QMutexLocker locker(&d->mutex);

    bool result = d->timerList.unregisterTimers(object);

    // Note that we only notify the AUX thread when there is nothing to wait
    // for, as otherwise it's harmless to wait for the next timer or socket.
    if (result && d->timerList.isEmpty() && d->socketNotifiers.isEmpty())
        d->notifyThread();

    return result;
}

QList<QEventDispatcherOS2::TimerInfo>
QEventDispatcherOS2::registeredTimers(QObject *object) const
{
#ifndef QT_NO_DEBUG
    if (!object) {
        qWarning("QEventDispatcherOS2:registeredTimers: invalid argument");
        return QList<TimerInfo>();
    }
#endif

    Q_D(const QEventDispatcherOS2);

    // NOTE: no need to lock the mutex as the AUX thread never changes timerList.

    return d->timerList.registeredTimers(object);
}

int QEventDispatcherOS2::remainingTime(int timerId)
{
#ifndef QT_NO_DEBUG
    if (timerId < 1) {
        qWarning("QEventDispatcherOS2::remainingTime: invalid argument");
        return -1;
    }
#endif

    Q_D(QEventDispatcherOS2);

    // NOTE: no need to lock the mutex as the AUX thread never changes timerList.

    return d->timerList.timerRemainingTime(timerId);
}

void QEventDispatcherOS2::wakeUp()
{
    Q_D(QEventDispatcherOS2);
    WinPostQueueMsg(d->hmq, WM_NULL, 0, 0);
}

void QEventDispatcherOS2::interrupt()
{
    Q_D(QEventDispatcherOS2);
    d->interrupt.store(1);
    wakeUp();
}

void QEventDispatcherOS2::flush()
{
}

void QEventDispatcherOS2::startingUp()
{
    Q_D(QEventDispatcherOS2);
    d->createMsgQueue();
}

void QEventDispatcherOS2::closingDown()
{
    Q_D(QEventDispatcherOS2);
    d->destroyMsgQueue();
}

QT_END_NAMESPACE
