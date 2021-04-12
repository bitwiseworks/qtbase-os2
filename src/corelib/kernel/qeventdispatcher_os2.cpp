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

#define WM_QT_TIMER_OR_SOCKET WM_SEM3
#define WM_QT_WAKEUP WM_SEM4

// This should speed things up a bit when logging is disabled.
static const bool lcCoreEventsDebug = lcCoreEvents().isDebugEnabled();
#define TRACE(m) do { if (Q_UNLIKELY(lcCoreEventsDebug)) qCDebug(lcCoreEvents) << m; } while(0)
#define V(v) #v << v

extern uint qGlobalPostedEventsCount();

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
    void prepareForSelect(bool wakeUp = false);
    int processTimersAndSockets();

    void startThread();
    void stopThread();

    inline void maybeStartThread() {
        if (!socketNotifiers.isEmpty() || timerList.count() > timerList.zeroTimerCount())
            startThread();
    }

    inline void maybeStopThread() {
        if (tid != -1 && socketNotifiers.isEmpty() && timerList.count() == timerList.zeroTimerCount())
            stopThread();
    }

    inline void maybeStopOrStartThread() {
        if (tid != -1) {
            if (socketNotifiers.isEmpty() && timerList.count() == timerList.zeroTimerCount())
                stopThread();
            else
                startThread();
        }
    }

    static void threadMain(void *arg);

    HAB hab;
    HMQ hmq;

    // Auxiliary object window to process WM_U_SEM_SELECT and WM_U_SEM_TIMER messages.
    // We need a dedicated window along with processing these messages directly in
    // QEventLoop::processEvents() to make sure they are processed even if the
    // current message loop is not run by Qt. This happens when a native modal
    // dialog is shown or when a top-level Qt widget is being moved or resized
    // using the mouse, or when a Qt-based DLL plugin is used by a non-Qt
    // application.
    class AuxWnd : public QPMObjectWindow
    {
    public:
        AuxWnd(QEventDispatcherOS2Private &that) : QPMObjectWindow(true /*deferred*/), d(that) {}
        MRESULT message(ULONG msg, MPARAM mp1, MPARAM mp2);
    private:
        QEventDispatcherOS2Private &d;
    } auxWnd;

    QAtomicInt interrupt; // bool

    QList<QMSG> queuedUserInputEvents;
    bool queuedSockets : 1;

    QHash<int, QSocketNotifierSetOS2> socketNotifiers;
    QSet<QSocketNotifier *> pendingNotifiers;

    QTimerInfoList timerList;
    bool processedTimers : 1;

    QEventLoop::ProcessEventsFlags flags;

    QWaitCondition cond;

    int tid;

    mutable QMutex mutex; // guards access to further variables

    enum {
        Starting,
        Selecting,
        Canceling,
        Waiting,
        Terminating,
    } threadState;

    union
    {
        int maxfd; // used Selecting state
        int nsel; // used in Waiting state
    };

    static constexpr int SetSize = 3;

    struct {
        QSocketNotifier::Type type;
        fd_set set;
    } fdsets[SetSize];

    timeval *timeout;
    timeval waitTime;
};

MRESULT QEventDispatcherOS2Private::AuxWnd::message(ULONG msg, MPARAM /*mp1*/, MPARAM /*mp2*/)
{
    // NOTE: Implementing socket thread synchronization (and data exchange) via a PM message
    // guarantees that select is not run outside the scope of processEvents (the select thread will
    // be sleeping until WM_QT_TIMER_OR_SOCKET gets processed either by processEvents or by the
    // native PM message loop). If there is no select running, there are no new timer/socket events
    // being sent. This seems to be expected by Qt and, in particular,
    // tst_QSocketNotifier::posixSockets depends on this.

    if (msg == WM_QT_TIMER_OR_SOCKET) {
        // Must get a lock to make sure the select thread has entered the wait state.
        QMutexLocker locker(&d.mutex);

        TRACE(V(d.tid) << V(d.threadState) << "maxfd/nsel" << d.maxfd << Qt::hex << "status" << WinQueryQueueStatus(HWND_DESKTOP));
        Q_ASSERT(d.threadState == Waiting);

        d.fetchFromSelect();

        // It's important to let the select thread go again before calling #processTimersAndSockets
        // because events it sends may enter a nested event loop depending on timers/sockets.
        d.prepareForSelect(true /*wakeUp*/);

        // Release the lock before #processTimersAndSockets to avoid deadlocks in cases when events
        // sent by it cause timer/socket creation/removal (which also needs this lock).
        locker.unlock();

        d.processTimersAndSockets();

        // Since #processTimersAndSockets delivers all pending socket events (unblocking them for
        // further selection), the current socket set needs to be be sent to the select thread
        // again. This is covered if this pending event processing also registers or unregisters
        // sockets or timers, but if not, it will be covered here.
        d.maybeStartThread();
    }

    return 0;
}

QEventDispatcherOS2Private::QEventDispatcherOS2Private()
    : hab(NULLHANDLE), hmq(NULLHANDLE), auxWnd(*this), interrupt(false), queuedSockets(false), processedTimers(false)
    , tid(-1), threadState(Terminating), maxfd(-1), timeout(nullptr)
{
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

    TRACE(Qt::hex << V(hab) << V(hmq));
}

void QEventDispatcherOS2Private::destroyMsgQueue()
{
    TRACE(Qt::hex << V(hab) << V(hmq) << V(tid) << V(threadState));

    if (tid != -1)
        stopThread();

    if (auxWnd.hwnd() != NULLHANDLE)
        auxWnd.destroy();

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
    Q_ASSERT(threadState == Waiting);

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
void QEventDispatcherOS2Private::prepareForSelect(bool wakeUp)
{
    TRACE(V(wakeUp) << V(tid) << V(threadState) << "maxfd/nsel" << maxfd);
    Q_ASSERT(threadState == Starting || threadState == Waiting);

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

    // Inform the select thread we have new data. Note that there may be no new data because all
    // exising sockets are pending and there are no timers. In this case we will leave the select
    // thread Waiting: #maybeStartThread called on any socket/timer change (and also after
    // delivering pending socket events) in AuxWnd::message) will take care of that.
    if (wakeUp && threadState == Waiting && (maxfd >= 0 || timeout))
        cond.wakeOne();
}

int QEventDispatcherOS2Private::processTimersAndSockets()
{
    TRACE("exclude sockets" << !!(flags & QEventLoop::ExcludeSocketNotifiers));

    int n_activated = 0;

    processedTimers = true;

    if (!timerList.isEmpty())
        n_activated = timerList.activateTimers();

    if (!(flags & QEventLoop::ExcludeSocketNotifiers) && !pendingNotifiers.isEmpty()) {
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

void QEventDispatcherOS2Private::startThread()
{
    Q_ASSERT(!socketNotifiers.isEmpty() || timerList.count() > timerList.zeroTimerCount());

    QMutexLocker locker(&mutex);

    TRACE(V(tid) << V(threadState) << "maxfd/nsel" << maxfd << Qt::hex << "status" << WinQueryQueueStatus(HWND_DESKTOP));

    if (tid == -1 || threadState == Starting) {
        if (tid == -1) {
            // No thread, start it now.
            if (auxWnd.hwnd() == NULLHANDLE)
                auxWnd.create();
            tid = _beginthread(threadMain, nullptr, 0, this);
            if (tid == -1) {
                qErrnoWarning(errno, "QEventDispatcherOS2Private: _beginthread failed");
                return;
            }
            threadState = Starting;
        } else {
            // It's there but didn't start working yet, just let prepareForSelect update the data.
            maxfd = -1;
        }
        prepareForSelect();
    } else {
        if (threadState == Selecting) {
            // Cancel the current job to force Waiting state transition to pick up new data.
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
        } else if (threadState == Waiting) {
            // We end up here either from the WM_QT_TIMER_OR_SOCKET handler that wants to update
            // socket sets for select after delivering pending socket events or when the number
            // of socket notifiers changes and the select thread is Waiting because all old sockets
            // were pending (see #prepareForSelect). Let it go with new data unless there is
            // WM_QT_TIMER_OR_SOCKET already pending which will do the same.
            Q_STATIC_ASSERT(WM_QT_TIMER_OR_SOCKET == WM_SEM3);
            if (!(WinQueryQueueStatus(HWND_DESKTOP) & (QS_SEM3 << 16))) {
                // Let prepareForSelect update the data.
                maxfd = -1;
                prepareForSelect(true);
            }
        }
    }
}

void QEventDispatcherOS2Private::stopThread()
{
    QMutexLocker locker(&mutex);

    TRACE(V(tid) << V(threadState) << "maxfd/nsel" << maxfd);
    Q_ASSERT(tid != -1 && threadState != Terminating);

    if (threadState == Selecting) {
        // Terminate the current job.
        threadState = Terminating;
        if (maxfd >= 0) {
            // Terminate select() execution.
            int rc = ::so_cancel(maxfd);
            if (rc == -1)
                qErrnoWarning(errno, "QEventDispatcherOS2Private: so_cancel failed");
        } else {
            // Terminate the simple timer wait state.
            cond.wakeOne();
        }
    } else {
        if (threadState == Waiting) {
            // Terminate the wait for new data state.
            threadState = Terminating;
            cond.wakeOne();
        } else {
            Q_ASSERT(threadState == Starting || threadState == Canceling);
            threadState = Terminating;
        }
    }

    locker.unlock();

    // Wait for the thread to stop.
    TID tid_os2 = tid;
    APIRET arc = DosWaitThread(&tid_os2, DCWW_WAIT);
    if (arc)
        qWarning("QEventDispatcherOS2Private: DosWaitThread failed with %ld", arc);

    // The stop request may arrive before WM_QT_TIMER_OR_SOCKET gets a chance to be processed.
    // Remove it to ensire it's not processed when the select thread is not in Waiting state.
    // Note: no need to do it in a loop because it's a cumulative message.
    QMSG msg;
    WinPeekMsg(hab, &msg, 0, WM_QT_TIMER_OR_SOCKET, WM_QT_TIMER_OR_SOCKET, PM_REMOVE);

    tid = -1;
}

// static
void QEventDispatcherOS2Private::threadMain(void *arg)
{
    QEventDispatcherOS2Private *d = reinterpret_cast<QEventDispatcherOS2Private *>(arg);

    QMutexLocker locker(&d->mutex);

    TRACE(V(d->threadState));

    while (d->threadState != Terminating) {
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

                // Check for the cancel request. Note that if so_cancel was called after we
                // returned from select, it will be remembered and immediately interrupt any
                // subsequent select which is unwanted and should be ignored. We detect this
                // situation using explicit state checks.
                if (nsel == -1 && err == EINTR && d->threadState != Canceling && d->threadState != Terminating)
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
        } else if (timeout) {
            // No sockets, simply wait for the next timer.
            unsigned long msecs = timeout->tv_sec * 1000ul + timeout->tv_usec / 1000ul;
            d->cond.wait(locker.mutex(), msecs);
        }

        // Store the overall result.
        d->nsel = nsel;

        TRACE(V(d->threadState) << V(d->nsel));

        if (d->threadState != Terminating) {
            d->threadState = Waiting;
            // Inform the event dispatcher we have sockets or timers (unless it was a dummy run
            // because of e.g. two #prepareForSelect(true) calls in a row w/o giving this thread
            // time slices for a chance to exit the previous Waiting state in #wait below).
            if (maxfd >= 0 || timeout)
                WinPostMsg(d->auxWnd.hwnd(), WM_QT_TIMER_OR_SOCKET, NULL, NULL);
            // Wait until we are given new data.
            d->cond.wait(locker.mutex());
        }
    }

    // Reset maxfd to its initial state.
    d->maxfd = -1;

    TRACE("END");
}

QEventDispatcherOS2::QEventDispatcherOS2(QObject *parent)
    : QAbstractEventDispatcher(*new QEventDispatcherOS2Private, parent)
{
}

QEventDispatcherOS2::~QEventDispatcherOS2()
{
    // Destroy the message queue as late as possible (see #closingDown).
    Q_D(QEventDispatcherOS2);
    d->destroyMsgQueue();
}

bool QEventDispatcherOS2::processEvents(QEventLoop::ProcessEventsFlags flags)
{
    Q_D(QEventDispatcherOS2);

    TRACE(V(flags));

    Q_ASSERT(d->hmq);

    d->interrupt.storeRelaxed(false);

    emit awake();

    auto threadData = d->threadData.loadRelaxed();
    bool canWait;
    bool retVal = false;

    QMSG waitMsg;
    bool haveWaitMsg = false;

    // Memorize new flags for window procs called by WinDispatchMsg and for processTimersAndSockets.
    QEventLoop::ProcessEventsFlags oldFlags = d->flags;
    d->flags = flags;

    d->processedTimers = false;

    // We need to return true if we processed any posted events (some tests, e.g.
    // tst_QEventDispatcher::sendPostedEvents) expect that. See qGlobalPostedEventsCount and
    // QCoreApplicationPrivate::sendPostedEvents for more info on the below code.
    uint postedCount = threadData->postEventList.size() - threadData->postEventList.startOffset;
    TRACE(V(postedCount));
    if (postedCount) {
        QCoreApplicationPrivate::sendPostedEvents(0, 0, threadData);
        // Below, we use the following logic to detect if any posted events were *actually* sent.
        // If, after calling sendPostedEvents we still have some posted events, this means that
        // either processing these events resulted in new posted events (threadData->canWait will
        // be false in this case) or some events were QEvent::DeferredDelete and they were reposted
        // by sendPostedEvents (for processing later on an outer event loop). Such a synthetic
        // reposting should *not* prevent this event loop from entering the wait state - otherwise
        // it will spin in a tight cycle with 100% CPU load until terminated. To detect if there
        // were any "normal" events besides DeferredDelete, we compare the number of posted events
        // before and after. If threadData->canWait is false, there is no way to detect if we
        // sent any "normal" posted event or not (w/o changing the QCoreApplicationPrivate code).
        // Just pretend that we did hoping it will not break anything else. Note that we call
        // canWaitLocked *after* grabbing the new posted event count (to keep them in sync).
        uint postedCountAfter = threadData->postEventList.size() - threadData->postEventList.startOffset;
        bool canWaitLocked = threadData->canWaitLocked();
        TRACE(V(postedCountAfter) << V(canWaitLocked));
        if (canWaitLocked) {
            Q_ASSERT(postedCount >= postedCountAfter);
            // If before is greater than after, then we sent "normal" posted events.
            retVal = postedCount > postedCountAfter;
        } else {
            // Pretend we sent some "normal" posted event.
            retVal = true;
        }
    }

    do {
        bool quit = false;

        while (!d->interrupt.loadRelaxed()) {
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
                TRACE(Qt::hex << V(msg.msg) << "status" << WinQueryQueueStatus(HWND_DESKTOP));

                if (msg.msg == WM_QUIT) {
                    if (QCoreApplication::instance()) {
                        QCoreApplication::instance()->quit();
                        retVal = true;
                    } else {
                        retVal = false;
                    }
                    quit = true;
                    break;
                } else if (msg.msg == WM_QT_WAKEUP) {
                    // This is a handled wakeUp request, just ignore it.
                    break;
                }

                if (!filterNativeEvent(QByteArrayLiteral("os2_generic_QMSG"), &msg, 0)) {
                    WinDispatchMsg(d->hab, &msg);
                    // Qt (and tst_QTimer::timerFiresOnlyOncePerProcessEvents) requires that timers
                    // are run only once per processEvents invocation.
                    if (msg.msg == WM_QT_TIMER_OR_SOCKET) {
                        retVal = true;
                        break;
                    }
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
                   && (flags.testFlag(QEventLoop::WaitForMoreEvents))
                   && threadData->canWaitLocked()
                   && !d->interrupt.loadRelaxed());
        TRACE(V(canWait) << V(retVal) <<
              "zeroTimerCount" << d->timerList.zeroTimerCount() <<
              "WaitForMoreEvents" << !!(flags & QEventLoop::WaitForMoreEvents) <<
              "canWaitLocked" << threadData->canWaitLocked() <<
              "interrupt" << d->interrupt.loadRelaxed());
        if (canWait) {
            emit aboutToBlock();
            WinGetMsg(d->hab, &waitMsg, 0, 0, 0);
            haveWaitMsg = true;
            emit awake();
        }
    } while (canWait);

    // Qt (and tst_QTimer::timerFiresOnlyOncePerProcessEvents) requires that timers
    // are run only once per processEvents invocation.
    if (!d->processedTimers && d->processTimersAndSockets())
        retVal = true;

    // Restore previous flags.
    d->flags = oldFlags;

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
        qWarning("QEventDispatcherOS2: socket fd is not in [0, FD_SETSIZE]");
        return;
    } else if (notifier->thread() != thread() || thread() != QThread::currentThread()) {
        qWarning("QEventDispatcherOS2: socket notifiers cannot be enabled from another thread");
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

    d->maybeStartThread();
}

void QEventDispatcherOS2::unregisterSocketNotifier(QSocketNotifier *notifier)
{
    Q_ASSERT(notifier);
    int sockfd = notifier->socket();
    QSocketNotifier::Type type = notifier->type();
#ifndef QT_NO_DEBUG
    if (sockfd < 0 || unsigned(sockfd) >= FD_SETSIZE) {
        qWarning("QEventDispatcherOS2: socket fd is not in [0, FD_SETSIZE]");
        return;
    } else if (notifier->thread() != thread() || thread() != QThread::currentThread()) {
        qWarning("QEventDispatcherOS2: socket notifier (fd %d) cannot be disabled from another thread.\n"
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

    // Note: Not maybeStopThread since if there are still some sockets, we need to poke the select
    // thread instead of doing nothing - to let it pick up the new fd set (otherwise it will most
    // likely abort with ENOTSOCK due to a closed file handle).
    d->maybeStopOrStartThread();
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

    d->maybeStartThread();
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

    bool ok = d->timerList.unregisterTimer(timerId);

    // Note: Not maybeStopOrStartThread since no need to interrupt the select thread as only the
    // existing timers will be fired anyway.
    d->maybeStopThread();

    return ok;
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

    bool ok = d->timerList.unregisterTimers(object);

    // Note: Not maybeStopOrStartThread since no need to interrupt the select thread as only the
    // existing timers will be fired anyway.
    d->maybeStopThread();

    return ok;
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
    WinPostQueueMsg(d->hmq, WM_QT_WAKEUP, 0, 0);
}

void QEventDispatcherOS2::interrupt()
{
    Q_D(QEventDispatcherOS2);
    d->interrupt.storeRelaxed(true);
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
    // QGuiApplication calls this too early, before letting QCoreApplication call
    // post routines (i.e. those added with qAddPostRoutine). These routines may
    // still want to use the event loop so we should keep it working and postpone
    // destroyMsgQueue until our own destruction. Note that QApplication is free
    // from this problem as it calls post routines on its own before this method.
}

/*****************************************************************************
  Auxiliary object window class for dedicated message processing.
 *****************************************************************************/

/*!
    \class QPMObjectWindow

    The QPMObjectWindow class is an auxiliary class for dedicated message
    processing. Its functionality is based on PM object windows. Once an
    instance of this class is created, PM window messages can be sent or posted
    to it using send() or post() methods. Subclasses should implement the
    message() method to process sent or posted messages. The hwnd() method is
    used whenever a PM window handle of this object window is necessary to be
    passed as a HWND argument to other calls and/or messages.

    Instances of this class may be created only on the main GUI thread or on a
    thread that has created a PM message queue and runs the PM message loop
    \b itself. If you create an instance on a thread other than main, make sure
    you destroy it before destroying the thread's message queue.

    \note WM_CREATE and WM_DESTROY messages are processed internally and not
    delivered do the message() method. Instead, you can use the constructor and
    the destructor of the subclass, respectively.

    \note This class is OS/2 specific and not available in Qt for other
    platforms!
*/

/*!
    Constructs a new object window for the current thread.
    If \a deferred is \c false, this method calls create() to create a PM object
    window. Otherwise, you must call create() yourself before this object window
    is able to process messages.
*/
QPMObjectWindow::QPMObjectWindow(bool deferred /* = false */) :
    w(NULLHANDLE)
{
    if (!deferred)
        create();
}

/*!
    Destroys this object window.
    This method calls destroy() to free the PM object window.
*/
QPMObjectWindow::~QPMObjectWindow()
{
    destroy();
}

/*!
    Creates a PM object window.
    Returns \c true on success or \c false otherwise.
    The method does nothing but returns \c false if the window has been already
    created. The handle of the successfully created object window can be
    obtained using the hwnd() method.

    \note Must be called on the same thread that cosnstructed this instance.
*/
bool QPMObjectWindow::create()
{
    if (w != NULLHANDLE)
        return false;

    static const char *ClassName = "Qt5.ObjectWindow";
    static bool classRegistered = FALSE;

    if (!classRegistered) {
        WinRegisterClass(0, ClassName, windowProc, 0, QWL_USER + sizeof(PVOID));
        classRegistered = true;
    }

    w = WinCreateWindow(HWND_OBJECT, ClassName,
                        NULL, 0, 0, 0, 0, 0, NULLHANDLE,
                        HWND_BOTTOM, 0, this, nullptr);
    if (w == NULLHANDLE)
        qFatal("WinCreateWindow failed with 0x%08lX", WinGetLastError(0));

    return w != NULLHANDLE;
}

/*!
    Destroys the PM object window.
    Returns \c TRUE on success or \c FALSE otherwise.
    The method does nothing but returns \c FALSE  if the window has been
    already destroyed (or never created).

    \note Must be called on the same thread that cosnstructed this instance.
*/
bool QPMObjectWindow::destroy()
{
    if (w == NULLHANDLE)
        return false;

    HWND h = w;
    w = NULLHANDLE; // tell windowProc() we're unsafe
    WinDestroyWindow(h);

    return true;
}

//static
MRESULT EXPENTRY QPMObjectWindow::windowProc(HWND hwnd, ULONG msg,
                                             MPARAM mp1, MPARAM mp2)
{
    if (msg == WM_CREATE) {
        QPMObjectWindow *that = static_cast<QPMObjectWindow *>(mp1);
        if (!that)
            return (MRESULT) TRUE;
        WinSetWindowPtr(hwnd, QWL_USER, that);
        return (MRESULT) FALSE;
    }

    QPMObjectWindow *that =
        static_cast<QPMObjectWindow *>(WinQueryWindowPtr(hwnd, QWL_USER));
    Q_ASSERT(that);

    // Note: before WinCreateWindow() returns to the constructor or after the
    // destructor has been called, w is 0. We use this to determine that the
    // object is in the unsafe state (VTBL is not yet initialized or has been
    // already uninitialized), so message() points to never-never land.
    if (!that || !that->w)
        return (MRESULT) FALSE;

    return that->message(msg, mp1, mp2);
}

/*!
    \fn QPMObjectWindow::hwnd() const

    Returns a handle of the object window created by create(). Returns 0 if
    create() was never called or if the creation attempt failed.
*/

/*!
    \fn QPMObjectWindow::send(ULONG msg, MPARAM mp1, MPARAM mp2) const

    Synchronously sends a message \a msg with the given parameters \a mp1 and
    \a mp2 to this window handle and returns a reply from the message() function.

    \note Must be called on the same thread that cosnstructed this instance.
*/

/*!
    \fn QPMObjectWindow::post(ULONG msg, MPARAM mp1, MPARAM mp2) const

    Asynchronously posts a message \a msg with the given parameters \a mp1 and
    \a mp2 to this window handle. Returns \c true on success and \c false
    otherwise.

    \note Can be called on any thread.
*/

/*!
    \fn QPMObjectWindow::message(ULONG msg, MPARAM mp1, MPARAM mp2)

    This method is called whenever a message \a msg with parameters \a mp1 and
    \a mp2 is received by this object window. Every subclass should implement
    this method to do actual message processing.
*/

QT_END_NAMESPACE
