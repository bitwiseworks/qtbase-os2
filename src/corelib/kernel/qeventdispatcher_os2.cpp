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

#include "qloggingcategory.h"

QT_BEGIN_NAMESPACE

Q_LOGGING_CATEGORY(lcCoreEvents, "qt.core.events", QtWarningMsg)

// This should speed things up a bit when logging is disabled.
static const bool lcCoreEventsDebug = lcCoreEvents().isDebugEnabled();
#define TRACE(m) do { if (Q_UNLIKELY(lcCoreEventsDebug)) qCDebug(lcCoreEvents) << m; } while(0)
#define V(v) #v << v

extern uint qGlobalPostedEventsCount();

static ULONG WM_QT_TIMER_OR_SOCKET = 0;

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

    void fetchFromSelect();
    void prepareForSelect();
    int processTimersAndSockets(bool actSockets);

    bool startThread();
    void stopThread();

    static void threadMain(void *arg);

    HAB hab;
    HMQ hmq;

    QAtomicInt interrupt; // bool

    QList<QMSG> queuedUserInputEvents;
    bool queuedSockets;

    QHash<int, QSocketNotifierSetOS2> socketNotifiers;
    QSet<QSocketNotifier *> pendingNotifiers;

    QTimerInfoList timerList;

    QWaitCondition cond;

    mutable QMutex mutex; // guards access to further variables

    int tid;

    enum {
        Selecting,
        Canceling,
        Posted,
    } threadState;

    union
    {
        int maxfd; // used Selecting state
        int nsel; // used in Posted state
    };

    uint threadUsage;

    static constexpr int SetSize = 3;

    struct {
        QSocketNotifier::Type type;
        fd_set set;
    } fdsets[SetSize];

    timeval *timeout;
    timeval waitTime;
};

QEventDispatcherOS2Private::QEventDispatcherOS2Private()
    : hab(NULLHANDLE), hmq(NULLHANDLE), interrupt(false), queuedSockets(false)
    , tid(-1), threadState(Posted), maxfd(-1), threadUsage(0), timeout(nullptr)
{
    if (WM_QT_TIMER_OR_SOCKET == 0)
        WM_QT_TIMER_OR_SOCKET = WinAddAtom(WinQuerySystemAtomTable(), "QEventDispatcherOS2:WM_QT_TIMER_OR_SOCKET");

    fdsets[0].type = QSocketNotifier::Read;
    fdsets[1].type = QSocketNotifier::Write;
    fdsets[2].type = QSocketNotifier::Exception;

    for (int i = 0; i < SetSize; ++i)
        FD_ZERO(&fdsets[i].set);

    waitTime = { 0, 0 };
}

QEventDispatcherOS2Private::~QEventDispatcherOS2Private()
{
    Q_ASSERT(hmq == NULLHANDLE);
    Q_ASSERT(tid == -1);
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

    TRACE(hex << V(hab) << V(hmq));
}

void QEventDispatcherOS2Private::destroyMsgQueue()
{
    TRACE(hex << V(hab) << V(hmq) << V(tid) << V(threadState) << V(threadUsage));

    Q_ASSERT(tid == -1);

    if (hmq != NULLHANDLE) {
        WinDestroyMsgQueue(hmq);
        WinTerminate(hab);
        hmq = NULLHANDLE;
        hab = NULLHANDLE;
    }
}

// NOTE: Must be called from the mutex lock.
void QEventDispatcherOS2Private::fetchFromSelect()
{
    TRACE(V(threadState) << "maxfd/nsel" << maxfd);

    Q_ASSERT(threadState == Posted);

    if (nsel > 0) {
        // Find out which sockets have been reported and mark them as pending.
        for (auto it = socketNotifiers.cbegin(); it != socketNotifiers.cend(); ++it) {
            const QSocketNotifierSetOS2 &sn_set = it.value();
            for (int i = 0; i < SetSize; ++i) {
                QSocketNotifier *sn = sn_set.notifiers[fdsets[i].type];
                if (sn && FD_ISSET(sn->socket(), &fdsets[i].set) && !pendingNotifiers.contains(sn)) {
                    TRACE("PENDING" << V(sn->socket()) << V(sn->type()));
                    pendingNotifiers << sn;
                }
            }
        }
    }

    nsel = -1;
}

// NOTE: Must be called from the mutex lock.
void QEventDispatcherOS2Private::prepareForSelect()
{
    TRACE(V(tid) << V(threadState) << "maxfd/nsel" << maxfd);

    Q_ASSERT(tid == -1 || threadState == Posted);

    // Check that fetchFromSelect has been called so we don't overwrite the previous results.
    Q_ASSERT(maxfd == -1);

    // Reset socket sets.
    if (!socketNotifiers.isEmpty())
        for (int i = 0; i < SetSize; ++i)
            FD_ZERO(&fdsets[i].set);

    // Fill up socket sets with new data.
    for (auto it = socketNotifiers.cbegin(); it != socketNotifiers.cend(); ++it) {
        const QSocketNotifierSetOS2 &sn_set = it.value();
        for (int i = 0; i < SetSize; ++i) {
            QSocketNotifier *sn = sn_set.notifiers[fdsets[i].type];
            // NOTE: ignore sockets that are already pending to avoid polluting the
            // PM message queue with socket messages if the target window is not fast
            // enough to process them. This will also ensure that this thread won't be
            // spinning in a tight loop because of immediate select returns.
            if (sn && !pendingNotifiers.contains(sn)) {
                TRACE("SELECT" << V(sn->socket()) << V(sn->type()));
                int sockfd = sn->socket();
                FD_SET(sockfd, &fdsets[i].set);
                // memorize the highest fd
                if (maxfd < sockfd)
                    maxfd = sockfd;
            }
        }
    }

    // Get the maximum time we can wait.
    timeout = nullptr;
    waitTime = { 0l, 0l };
    timespec waitTimeTS = { 0l, 0l };
    if (timerList.timerWait(waitTimeTS)) {
        waitTime = timespecToTimeval(waitTimeTS);
        timeout = &waitTime;
    }

    // Inform the thread we have new data.
    if (tid != -1)
        cond.wakeOne();
}

int QEventDispatcherOS2Private::processTimersAndSockets(bool actSockets)
{
    TRACE(V(actSockets));

    int n_activated = 0;

    if (!timerList.isEmpty())
        n_activated = timerList.activateTimers();

    if (actSockets && !pendingNotifiers.isEmpty()) {
        QEvent event(QEvent::SockAct);

        while (!pendingNotifiers.isEmpty()) {
            auto it = pendingNotifiers.begin();
            QSocketNotifier *notifier = *it;
            pendingNotifiers.erase(it);
            QCoreApplication::sendEvent(notifier, &event);
            ++n_activated;
        }
    }

    TRACE(V(n_activated));

    return n_activated;
}

bool QEventDispatcherOS2Private::startThread()
{
    // NOTE: We support nested start/stop of the select thread (with the help
    // of threadUsage) for cases when the Qt event loop is entered from a native
    // message handler (a dialog in response to a mouse click etc).

    if (!socketNotifiers.count() && timerList.count() == timerList.zeroTimerCount())
        return false;

    QMutexLocker locker(&mutex);

    TRACE(V(tid) << V(threadState) << V(threadUsage) << "maxfd/nsel" << maxfd);

    if (tid == -1 || threadState == Posted) {
        if (threadState == Posted)
            fetchFromSelect();
        prepareForSelect();
    }

    if (tid == -1) {
        tid = _beginthread(threadMain, nullptr, 0, this);
        if (tid == -1) {
            qErrnoWarning(errno, "QEventDispatcherOS2Private: _beginthread failed");
            return false;
        }
        Q_ASSERT(!threadUsage);
        threadUsage = 1;
    } else {
        ++threadUsage;
        Q_ASSERT(threadUsage >= 2);
    }

    return true;
}

void QEventDispatcherOS2Private::stopThread()
{
    QMutexLocker locker(&mutex);

    TRACE(V(tid) << V(threadState) << V(threadUsage) << "maxfd/nsel" << maxfd);

    Q_ASSERT(tid != -1);

    Q_ASSERT(threadUsage);
    --threadUsage;

    if (threadState == Selecting) {
        threadState = Canceling;
        if (maxfd >= 0) {
            // Terminate select() execution.
            int rc = ::so_cancel(maxfd);
            if (rc == -1)
                qErrnoWarning(errno, "QEventDispatcherOS2Private: so_cancel failed");
        } else {
            // Terminate the simple wait state.
            cond.wakeOne();
        }
        if (threadUsage) {
            // Wait for the thread to go to Posted state.
            locker.unlock();

            QMSG msg;
            WinGetMsg(hab, &msg, 0, WM_QT_TIMER_OR_SOCKET, WM_QT_TIMER_OR_SOCKET);

            locker.relock();
        }
    } else {
        Q_ASSERT(threadState == Posted);

        // Make sure there is no unhandled WM_QT_TIMER_OR_SOCKET in the queue
        // (we will run timer and socket activation anyway once return from here).
        QMSG msg;
        WinPeekMsg(hab, &msg, 0, WM_QT_TIMER_OR_SOCKET, WM_QT_TIMER_OR_SOCKET, PM_REMOVE);

        if (!threadUsage) {
            // Terminate the Posted wait state.
            cond.wakeOne();
        }
    }

    if (!threadUsage) {
        // Wait for the thread to stop.
        locker.unlock();

        TID tid_os2 = tid;
        APIRET arc = DosWaitThread(&tid_os2, DCWW_WAIT);
        if (arc)
            qWarning("QEventDispatcherOS2Private: DosWaitThread failed with %ld", arc);

        locker.relock();

        tid = -1;
    }

    if (tid == -1 || threadState == Posted)
      fetchFromSelect();
}

// static
void QEventDispatcherOS2Private::threadMain(void *arg)
{
    QEventDispatcherOS2Private *d = reinterpret_cast<QEventDispatcherOS2Private *>(arg);

    QMutexLocker locker(&d->mutex);

    TRACE(V(d->threadState) << V(d->threadUsage));

    while (d->threadUsage) {
        d->threadState = Selecting;

        timeval *timeout = nullptr;
        timeval waitTime = { 0l, 0l };
        if (d->timeout) {
            waitTime = d->waitTime;
            timeout = &waitTime;
        }

        TRACE(V(timeout) << V(waitTime.tv_sec) << V(waitTime.tv_usec) << V(d->maxfd));

        int maxfd = d->maxfd;
        int nsel = 0;
        int err = 0;

        if (maxfd >= 0) {
            // We have some sockets to select.
            fd_set rdSet;
            fd_set wrSet;
            fd_set exSet;

            forever {
                rdSet = d->fdsets[0].set;
                wrSet = d->fdsets[1].set;
                exSet = d->fdsets[2].set;

                locker.unlock();

                nsel = ::select(maxfd + 1, &rdSet, &wrSet, &exSet, timeout);
                err = errno;

                TRACE(V(nsel) << V(err));

                if (nsel == -1 && err == EINVAL) {
                    qWarning("QSocketNotifier: select() returned EINVAL, make sure that "
                             "the socket is not an OS/2 file handle.");
                    // Give it some sleep to avoid 100% CPU load due to select()
                    // constantly failing in a tight loop which freezes the system.
                    DosSleep(100);
                }

                locker.relock();

                // Only stopThread is allowed to cancel, ignore all other requests
                // (e.g. so_cancel from the previous stopThread happening after select
                // which will inevitably immmediately break the next select).
                if (nsel == -1 && err == EINTR && d->threadState != Canceling)
                    continue;

                break;
            }

            // Copy the select results back.
            if (nsel > 0)
            {
                d->fdsets[0].set = rdSet;
                d->fdsets[1].set = wrSet;
                d->fdsets[2].set = exSet;
            }
        } else {
            // No sockets, simply wait for the next timer.
            unsigned long msecs = timeout ?
                timeout->tv_sec * 1000ul + timeout->tv_usec / 1000ul : ULONG_MAX;

            d->cond.wait(locker.mutex(), msecs);
        }

        // Store the overall result.
        d->nsel = nsel;

        TRACE(V(d->threadState) << V(d->threadUsage));

        d->threadState = Posted;

        if (d->threadUsage) {
            WinPostQueueMsg(d->hmq, WM_QT_TIMER_OR_SOCKET, NULL, NULL);

            // Wait until we are given new data.
            d->cond.wait(locker.mutex());
        }
    }

    TRACE("END");
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

    bool startedThread = false;

    TRACE(V(flags));

    QCoreApplicationPrivate::sendPostedEvents(0, 0, d->threadData);

    // Start the select thread.
    startedThread = d->startThread();

    TRACE(V(startedThread));

    do {
        bool quit = false;

        while (!d->interrupt.load()) {
            QMSG msg;
            bool haveMessage;

            if (!(flags & QEventLoop::ExcludeUserInputEvents) && !d->queuedUserInputEvents.isEmpty()) {
                // Process queued user input events.
                haveMessage = true;
                msg = d->queuedUserInputEvents.takeFirst();
            } else if(!(flags & QEventLoop::ExcludeSocketNotifiers) && d->queuedSockets) {
                // Process queued socket events.
                haveMessage = true;
                memset(&msg, 0, sizeof(msg));
                msg.msg = WM_QT_TIMER_OR_SOCKET;
                d->queuedSockets = false;
            } else {
                if (haveWaitMsg) {
                    haveMessage = true;
                    msg = waitMsg;
                    haveWaitMsg = false;
                } else {
                    haveMessage = WinPeekMsg(d->hab, &msg, 0, 0, 0, PM_REMOVE) == TRUE;
                    TRACE(V(haveMessage));
                }
                if (haveMessage) {
                    if ((flags & QEventLoop::ExcludeUserInputEvents) &&
                        (msg.msg == WM_CHAR ||
                         (msg.msg >= WM_MOUSEFIRST &&
                          msg.msg <= WM_MOUSELAST) ||
                         (msg.msg >= WM_EXTMOUSEFIRST &&
                          msg.msg <= WM_EXTMOUSELAST) ||
                         msg.msg == WM_HSCROLL ||
                         msg.msg == WM_VSCROLL)) {
                        // Queue user input events for later processing.
                        d->queuedUserInputEvents.append(msg);
                        continue;
                    } else if ((flags & QEventLoop::ExcludeSocketNotifiers) &&
                        (msg.msg == WM_QT_TIMER_OR_SOCKET)) {
                        // Queue socket events for later processing.
                        d->queuedSockets = true;
                        continue;
                    }
                }
            }
            if (haveMessage) {
                TRACE(hex << V(msg.msg));

                if (msg.msg == WM_QUIT) {
                    if (QCoreApplication::instance()) {
                        QCoreApplication::instance()->quit();
                        retVal = true;
                    } else {
                        retVal = false;
                    }
                    quit = true;
                    break;
                } else if (msg.msg == WM_QT_TIMER_OR_SOCKET) {
                    retVal = true;
                    break;
                } else if (msg.msg == WM_SEM1) {
                    // This is a handled wakeUp request, just ignore it.
                    break;
                }

                if (!filterNativeEvent(QByteArrayLiteral("os2_generic_QMSG"), &msg, 0)) {
                    WinDispatchMsg(d->hab, &msg);
                }
            } else {
                // Nothing todo, so break.
                break;
            }
            retVal = true;
        }

        if (quit)
            break;

        // Still nothing - wait for message.
        canWait = (!retVal
                   && !d->timerList.zeroTimerCount()
                   && (flags & QEventLoop::WaitForMoreEvents)
                   && d->threadData->canWaitLocked()
                   && !d->interrupt.load());
        TRACE(V(canWait));
        if (canWait) {
            emit aboutToBlock();
            WinGetMsg(d->hab, &waitMsg, 0, 0, 0);
            haveWaitMsg = true;
            emit awake();
        }
    } while (canWait);

    // Stop the select thread. This is necessary to guarantee that select is
    // not run beyond the scope of processEvents and no new notifications are
    // generated. In particular, tst_QSocketNotifier::posixSockets expects this.
    // Also, the Qt code (& tests) seems to assume that timers are not
    // processed more than once per a processEvents iteration.
    if (startedThread)
        d->stopThread();

    if (d->processTimersAndSockets(!(flags & QEventLoop::ExcludeSocketNotifiers)))
        retVal = true;

    TRACE(V(retVal));

    return retVal;
}

bool QEventDispatcherOS2::hasPendingEvents()
{
    QMSG msg;
    return qGlobalPostedEventsCount() || WinPeekMsg(0, &msg, NULLHANDLE, 0, 0, PM_NOREMOVE);
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

    TRACE(V(sockfd) << V(type));

    QSocketNotifierSetOS2 &sn_set = d->socketNotifiers[sockfd];

    if (sn_set.notifiers[type] && sn_set.notifiers[type] != notifier)
        qWarning("%s: Multiple socket notifiers for same socket %d and type %s",
                 Q_FUNC_INFO, sockfd, socketType(type));

    sn_set.notifiers[type] = notifier;
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

    TRACE(V(sockfd) << V(type));

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

    TRACE(V(timerId) << V(interval) << V(timerType) << V(object));

    d->timerList.registerTimer(timerId, interval, timerType, object);
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

    TRACE(V(timerId));

    return d->timerList.unregisterTimer(timerId);
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

    TRACE(V(object));

    return d->timerList.unregisterTimers(object);
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

    return d->timerList.timerRemainingTime(timerId);
}

void QEventDispatcherOS2::wakeUp()
{
    Q_D(QEventDispatcherOS2);
    WinPostQueueMsg(d->hmq, WM_SEM1, 0, 0);
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
