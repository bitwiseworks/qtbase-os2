/****************************************************************************
**
** Copyright (C) 2010 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: Nokia Corporation (qt-info@nokia.com)
**
** Copyright (C) 2010 netlabs.org. OS/2 parts.
**
** This file is part of the QtCore module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial Usage
** Licensees holding valid Qt Commercial licenses may use this file in
** accordance with the Qt Commercial License Agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Nokia.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights.  These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
**
** If you have questions regarding the use of this file, please contact
** Nokia at qt-info@nokia.com.
** $QT_END_LICENSE$
**
****************************************************************************/

#include "qeventdispatcher_pm_p.h"

#include "qcoreapplication.h"
#include "qhash.h"
#include "qsocketnotifier.h"
#include "qmutex.h"
#include "qwaitcondition.h"
#include "qlist.h"
#include "qmap.h"
#include "qhash.h"
#include "qset.h"

#include "qabstracteventdispatcher_p.h"
#include "qcoreapplication_p.h"
#include "qpmobjectwindow_pm_p.h"

#include <private/qthread_p.h>
#include <private/qmutexpool_p.h>

#include <sys/socket.h>
#include <sys/times.h> // for times()
#include <unistd.h> // for sysconf()

QT_BEGIN_NAMESPACE

extern uint qGlobalPostedEventsCount();

/*****************************************************************************
  Auxiliary object window class for dedicated message processing.
  Declared in qpmobjectwindow_pm_p.h
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

    static const char *ClassName = "QPMObjectWindow";
    static bool classRegistered = FALSE;

    if (!classRegistered) {
        WinRegisterClass(0, ClassName, windowProc, 0, QWL_USER + sizeof(PVOID));
        classRegistered = true;
    }

    w = WinCreateWindow(HWND_OBJECT, ClassName,
                        NULL, 0, 0, 0, 0, 0, NULL,
                        HWND_BOTTOM, 0, this, NULL);
    if (w == NULLHANDLE)
        qWarning("QPMObjectWindow::create: WinCreateWindow failed with 0x%08lX",
                 WinGetLastError(0));

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


// socket select notification (higher priority)
#define WM_U_SEM_SELECT     WM_USER
// timer notification (lower priority)
#define WM_U_SEM_TIMER      WM_TIMER

// Internal operator functions for timevals

inline bool operator<(const timeval &t1, const timeval &t2)
{
    return t1.tv_sec < t2.tv_sec || (t1.tv_sec == t2.tv_sec && t1.tv_usec < t2.tv_usec);
}

inline bool operator==(const timeval &t1, const timeval &t2)
{
    return t1.tv_sec == t2.tv_sec && t1.tv_usec == t2.tv_usec;
}

inline bool operator>=(const timeval &t1, const timeval &t2)
{
    return !operator<(t1, t2);
}

inline timeval &operator+=(timeval &t1, const timeval &t2)
{
    t1.tv_sec += t2.tv_sec;
    if ((t1.tv_usec += t2.tv_usec) >= 1000000l) {
        ++t1.tv_sec;
        t1.tv_usec -= 1000000l;
    }
    return t1;
}
inline timeval operator+(const timeval &t1, const timeval &t2)
{
    timeval tmp;
    tmp.tv_sec = t1.tv_sec + t2.tv_sec;
    if ((tmp.tv_usec = t1.tv_usec + t2.tv_usec) >= 1000000l) {
        ++tmp.tv_sec;
        tmp.tv_usec -= 1000000l;
    }
    return tmp;
}
inline timeval operator-(const timeval &t1, const timeval &t2)
{
    timeval tmp;
    tmp.tv_sec = t1.tv_sec - t2.tv_sec;
    if ((tmp.tv_usec = t1.tv_usec - t2.tv_usec) < 0l) {
        --tmp.tv_sec;
        tmp.tv_usec += 1000000l;
    }
    return tmp;
}

/*****************************************************************************
 socket select() and timer service thread
 *****************************************************************************/

#if !defined(QT_NO_THREAD)

class QSelectThread : public QThread
{
public:

    typedef QSocketNotifier::Type Type;

    static void addSelect(QSocketNotifier *notifier, HWND hwnd);
    static void removeSelect(QSocketNotifier *notifier);
    static QSocketNotifier *getSocketNotifier(int key, bool reset);

    // timer support is based on QTimerInfoList from the Unix implementation
    static void addTimer(int timerId, int interval, QObject *object, HWND hwnd);
    static bool removeTimer(int timerId);
    static bool removeTimers(QObject *object);
    static QList<QPair<int, int> > knownTimers(QObject *object);
    static QObject *getTimerObject(int timerId, bool reset);

    static void attachThread();
    static void detachThread();

private:
    QSelectThread();
    ~QSelectThread();

    enum Op { Add, Remove };
    void updateMaxSockFd(int sockFd, Op op);
    fd_set *setForType(QSocketNotifier::Type type);

    void run();
    void cancelSelectOrIdle();

    struct TimerInfo;

    bool timeChanged(timeval *delta);
    void getTime(timeval &t);
    void updateCurrentTime();
    void removeFromTimeoutMap(TimerInfo *ti);

    bool timerWait(timeval &);
    void timerRepair(const timeval &);

    void activateTimers();

    bool finish;
    int refcnt;
    QWaitCondition cond;

    // socket stuff

    typedef QPair<QSocketNotifier*, HWND> SockNot;
    typedef QHash<int, SockNot> Sockets;
    Sockets sockets;

    fd_set readS, writeS, exS;
    int maxSockfd, maxSockfdInSelect;

    // timer stuff

    struct TimerInfo {
        int id;           // - timer identifier
        timeval interval; // - timer interval
        timeval timeout;  // - when to sent event
        QObject *obj;     // - object to receive event
        HWND hwnd;        // - where to post the timer message
        bool posted;      // - true if a message has been posted
    };

    typedef QHash<int, TimerInfo*> TimerInfoMap;
    TimerInfoMap timers;

    typedef QMap<timeval, TimerInfo*> TimevalMap;
    TimevalMap timersByTimeout;
    bool allTimersPosted;

    timeval currentTime;
    timeval previousTime;
    clock_t previousTicks;
    int ticksPerSecond;
    int msPerTick;

    static int toSockKey(int sockfd, Type type) {
        // as a hash key, we use first two bits of int for the type and the rest
        // for the sockfd which should be enough in the real world
        Q_ASSERT(((sockfd << 2) >> 2) == sockfd);
        return (sockfd << 2) + type;
    }
    // opposite to toSockKey()
    static int toSocket(int key) { return key >> 2; }
    static Type toSockType(int key) { return Type(key & 0x3); }

    static QSelectThread *instance;
    static QMutex mutex;
};

// static
QSelectThread *QSelectThread::instance = 0;
QMutex QSelectThread::mutex;

// static
void QSelectThread::addSelect(QSocketNotifier *notifier, HWND hwnd)
{
    Q_ASSERT(hwnd != NULLHANDLE);
    if (hwnd == NULLHANDLE)
        return;

    int sockfd = notifier->socket();
    Type type = notifier->type();
    int key = toSockKey(sockfd, type);

    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    // lazy start
    instance->start();

    if (instance->sockets.contains(key)) {
        static const char *t[] = { "Read", "Write", "Exception" };
        qWarning("QSocketNotifier: Multiple socket notifiers for "
                 "same socket %d and type %s", sockfd, t[type]);

    }
    instance->sockets.insert(key, qMakePair(notifier, hwnd));
    FD_SET(sockfd, instance->setForType(type));
    instance->updateMaxSockFd(sockfd, Add);
    instance->cancelSelectOrIdle();
}

// static
void QSelectThread::removeSelect(QSocketNotifier *notifier)
{
    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    int sockfd = notifier->socket();
    Type type = notifier->type();
    int key = toSockKey(sockfd, notifier->type());

    if (instance->sockets.contains(key)) {
        instance->sockets.remove(key);
        FD_CLR(sockfd, instance->setForType(type));
        instance->updateMaxSockFd(sockfd, Remove);
        instance->cancelSelectOrIdle();
    }
}

/*!
    Returns the socket notifier object corresponding to the given key from the
    WM_U_SEM_SELECT message. The return value is only correct for the thread
    that owns QSocketNotifier (creates/registers/unregisters it).

    May return 0 if the socket notifier object is disabled/deleted after
    WM_U_SEM_SELECT was issued for it but before this message gets processed by
    the owning thread.
*/
// static
QSocketNotifier *QSelectThread::getSocketNotifier(int key, bool reset)
{
    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    if (instance->sockets.contains(key)) {
        QSocketNotifier* notifier = instance->sockets[key].first;
        if (notifier->thread() == QThread::currentThread()) {
            if (reset && notifier->isEnabled()) {
                // add the socket back to the set
                int sockfd = notifier->socket();
                FD_SET(sockfd, instance->setForType(notifier->type()));
                instance->updateMaxSockFd(sockfd, Add);
                // inform the select thread that this socket may be included
                // in the set and posted again
                instance->cancelSelectOrIdle();
            }
            return notifier;
        }
    }

    return 0;
}

// static
void QSelectThread::addTimer(int timerId, int interval, QObject *object, HWND hwnd)
{
    Q_ASSERT(hwnd != NULLHANDLE);
    if (hwnd == NULLHANDLE)
        return;

    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    // lazy start
    instance->start();

    instance->updateCurrentTime();

    TimerInfo *t = new TimerInfo;
    t->id = timerId;
    t->interval.tv_sec  = interval / 1000;
    t->interval.tv_usec = (interval % 1000) * 1000;
    t->timeout = instance->currentTime + t->interval;
    t->obj = object;
    t->hwnd = hwnd;
    t->posted = false;

    instance->timers.insert(t->id, t);
    // QMap is sorted by key (timeval), which is exactly what we need
    instance->timersByTimeout.insertMulti(t->timeout, t);

    instance->cancelSelectOrIdle();
}

// static
bool QSelectThread::removeTimer(int timerId)
{
    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    // set timer inactive
    TimerInfo *t = instance->timers.take(timerId);
    if (t) {
        // release the timer id
        if (!QObjectPrivate::get(t->obj)->inThreadChangeEvent)
            QAbstractEventDispatcherPrivate::releaseTimerId(timerId);
        instance->removeFromTimeoutMap(t);

        delete t;
        return true;
    }

    // id not found
    return false;
}

// static
bool QSelectThread::removeTimers(QObject *object)
{
    QMutexLocker locker(&mutex);
    // it is possible that QEventDispatcherPM::unregisterTimers() is called
    // after detaching the last thread with detachThread() which means all
    // timers are already removed and the select thread is stopped and deleted.
    if (!instance)
        return false;

    if (instance->timers.isEmpty())
        return false;
    for (TimerInfoMap::iterator it = instance->timers.begin();
          it != instance->timers.end();) {
        register TimerInfo *t = it.value();
        if (t->obj == object) {
            // object found
            it = instance->timers.erase(it);

            // release the timer id
            if (!QObjectPrivate::get(t->obj)->inThreadChangeEvent)
                QAbstractEventDispatcherPrivate::releaseTimerId(t->id);
            instance->removeFromTimeoutMap(t);

            delete t;
        } else {
            ++it;
        }
    }
    return true;
}

// static
QList<QPair<int, int> > QSelectThread::knownTimers(QObject *object)
{
    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    QList<QPair<int, int> > list;
    for (TimerInfoMap::const_iterator it = instance->timers.constBegin();
          it != instance->timers.constEnd(); ++it) {
        register const TimerInfo * const t = it.value();
        if (t->obj == object)
            list << QPair<int, int>(t->id, t->interval.tv_sec * 1000 + t->interval.tv_usec / 1000);
    }
    return list;
}

/*!
    Returns the timer object corresponding to the given timer ID from the
    WM_U_SEM_TIMER message. The return value is only correct for the thread
    that owns the timer (creates/registers/unregisters it).

    If @a reset is true, the method also resets the 'posted' flag so that a
    message for it may be posted again by the select thread.

    May return 0 if the timer is stopped/deleted after WM_U_SEM_TIMER was issued
    for it but before this message gets processed by the owning thread.
*/
// static
QObject *QSelectThread::getTimerObject(int timerId, bool reset)
{
    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    TimerInfo *t = instance->timers.value(timerId);
    if (t) {
        if (reset) {
            t->posted = false;
            if (instance->allTimersPosted) {
                // the select thread's been sleeping forever; wake it up -- we
                // now have at least one timer that can be posted again
                instance->allTimersPosted = false;
                instance->cancelSelectOrIdle();
            } else {
                // also wake it up if this timer was skipped when choosing the
                // shortest wait interval so that a longer one could be chosen
                bool haveNonPosted = false;
                for (TimevalMap::const_iterator it = instance->timersByTimeout.begin();
                     it != instance->timersByTimeout.end() && !haveNonPosted &&
                     it.value() != t;
                     ++it) {
                    if (!it.value()->posted)
                        haveNonPosted = true;
                }
                if (!haveNonPosted)
                    instance->cancelSelectOrIdle();
            }
        }
        return t->obj;
    }

    return 0;
}

/*!
    Incrreases the usage count of the QSelectThread instance by one. If no
    QSelectThread instance exists yet, creates it. Note that the thread is
    started on demand by addSelect(), not by this method.

    Must be called once per thread before any other call and must be completed
    by the detachThread() call when socket select functionality is no more
    necassary.
*/
// static
void QSelectThread::attachThread()
{
    QMutexLocker locker(&mutex);

    if (instance == 0) {
        instance = new QSelectThread();
    } else {
        while (instance->finish) {
            // the previous instance is still getting finished; let it go
            locker.unlock();
            QThread::yieldCurrentThread();
            locker.relock();
        }
    }

    // don't count on ourselves
    if (instance != QThread::currentThread())
        ++instance->refcnt;
}

/*!
    Removes all socket notifiers owned by the current thread and decreases the
    usage count of the QSelectThread instance by one. When the usage count
    goes to zero, the socket select thread is stopped and the instance is
    deleted.

    May only be called once per thread and only if attachThread() was called on
    this thread before.
*/
// static
void QSelectThread::detachThread()
{
    QMutexLocker locker(&mutex);

    // don't count on ourselves
    if (instance == QThread::currentThread())
        return;

    // remove socket notifiers owned by this thread
    for (Sockets::iterator it = instance->sockets.begin();
          it != instance->sockets.end();) {
        QSocketNotifier *notifier = it.value().first;
        if (notifier->thread() == QThread::currentThread()) {
            it = instance->sockets.erase(it);
            instance->updateMaxSockFd(notifier->socket(), Remove);
            FD_CLR(notifier->socket(), instance->setForType(notifier->type()));
        } else {
            ++it;
        }
    }

    // remove timers owned by this thread
    for (TimerInfoMap::iterator it = instance->timers.begin();
          it != instance->timers.end();) {
        register TimerInfo *t = it.value();
        if (t->obj->thread() == QThread::currentThread()) {
            // object found
            it = instance->timers.erase(it);

            // release the timer id
            if (!QObjectPrivate::get(t->obj)->inThreadChangeEvent)
                QAbstractEventDispatcherPrivate::releaseTimerId(t->id);
            instance->removeFromTimeoutMap(t);

            delete t;
        } else {
            ++it;
        }
    }

    if (--instance->refcnt == 0) {
        instance->finish = true;
        instance->cancelSelectOrIdle();
        // let the instance terminate and then delete it
        locker.unlock();
        instance->wait();
        locker.relock();
        delete instance;
        instance = 0;
    } else {
        instance->cancelSelectOrIdle();
    }
}

QSelectThread::QSelectThread()
    : finish(false), refcnt(0), allTimersPosted(false)
{
    // initialize socket stuff
    FD_ZERO(&readS);
    FD_ZERO(&writeS);
    FD_ZERO(&exS);
    maxSockfd = maxSockfdInSelect = -1;

    // initialize timer stuff
    getTime(currentTime);

    // initialize the timeChanged() machinery
    previousTime = currentTime;

    tms unused;
    previousTicks = times(&unused);

    ticksPerSecond = sysconf(_SC_CLK_TCK);
    msPerTick = 1000 / ticksPerSecond;
}

QSelectThread::~QSelectThread()
{
    // detachThread() calls must clean up sockets/timers they own
    Q_ASSERT(sockets.isEmpty());
    Q_ASSERT(timers.isEmpty());
    Q_ASSERT(timersByTimeout.isEmpty());

    // but, still, be nice and cleanup timers (we allocate them with new)
    qDeleteAll(timers);
}

// Finds the new highest socket FD. Must be called from under the mutex.
void QSelectThread::updateMaxSockFd(int sockFd, Op op)
{
    switch (op) {
    case Add:
        maxSockfd = qMax(maxSockfd, sockFd);
        break;
    case Remove:
        if (maxSockfd == sockFd) {
            maxSockfd = -1;
            if (!sockets.isEmpty()) {
                for (Sockets::const_iterator it = sockets.constBegin();
                      it != sockets.constEnd(); ++it) {
                    int fd = toSocket(it.key());
                    if (FD_ISSET(fd, &readS) || FD_ISSET(fd, &writeS) ||
                        FD_ISSET(fd, &exS))
                        maxSockfd = qMax(fd, maxSockfd);
                }
            }
        }
        break;
    }
}

// Returns the set corresponding to the socket type. Must be called from under
// the mutex.
fd_set *QSelectThread::setForType(QSocketNotifier::Type type)
{
    fd_set *set = 0;
    switch (type) {
        case QSocketNotifier::Read:
            set = &readS; break;
        case QSocketNotifier::Write:
            set = &writeS; break;
        case QSocketNotifier::Exception:
            set = &exS; break;
    }
    return set;
}

void QSelectThread::run()
{
    mutex.lock();

    do {
        // get the maximum time we can wait (for the closest timer)
        timeval *timeout = 0;
        timeval wait_tm = { 0l, 0l };
        if (timerWait(wait_tm))
            timeout = &wait_tm;

        // do select or simple wait
        int nsel = 0;
        if (maxSockfd >= 0) {
            fd_set tmpRead = readS, tmpWrite = writeS, tmpEx = exS;
            maxSockfdInSelect = maxSockfd;
            mutex.unlock();
            nsel = ::select(maxSockfdInSelect + 1, &tmpRead, &tmpWrite, &tmpEx,
                            timeout);
            if (nsel == -1 && errno == EINVAL) {
                qWarning("QSocketNotifier: select() returned EINVAL, check that "
                         "the socket is not an OS/2 file handle.");
                // give it some sleep to avoid 100% CPU load due to select()
                // constantly failing in a tight loop which freezes the system
                msleep(100);
            }
            mutex.lock();
            if (nsel > 0) {
                // find out which sockets to post. Note that we remove these
                // sockets from the main sets to avoid polluting the message
                // queue with sockett messages if the target window is not fast
                // enough to process them. They will be put back once processed.
                for (Sockets::const_iterator it = sockets.constBegin();
                      it != sockets.constEnd(); ++it) {
                    int sockfd = toSocket(it.key());
                    Type type = toSockType(it.key());
                    bool isSet = false;
                    switch (type) {
                        case QSocketNotifier::Read:
                            isSet = FD_ISSET(sockfd, &tmpRead); break;
                        case QSocketNotifier::Write:
                            isSet = FD_ISSET(sockfd, &tmpWrite); break;
                        case QSocketNotifier::Exception:
                            isSet = FD_ISSET(sockfd, &tmpEx); break;
                    }
                    if (isSet) {
                        // remove the socket from the set to avoid immediate
                        // select() returns in a tight loop (the thread that
                        // handles WM_U_SEM_SELECT will set add it back)
                        FD_CLR(sockfd, setForType(type));
                        updateMaxSockFd(sockfd, Remove);
                        WinPostMsg(it.value().second, WM_U_SEM_SELECT, MPFROMLONG(it.key()), 0);
                    }
                }
            }
        } else {
            nsel = -1;
            errno = EINTR; // indicate interrupt
            long msecs = timeout ?
                timeout->tv_sec * 1000l + timeout->tv_usec / 1000l : LONG_MAX;
            maxSockfdInSelect = -1;
            if (!cond.wait(&mutex, msecs))
                nsel = 0; // indicate timeout
        }

        if (nsel == 0 || (nsel == -1 && errno == EINTR)) {
            // interrupt or timeout; check if there are expired or processed
            // timers
            activateTimers();
        }

    } while(!finish);

    mutex.unlock();
}

// Must be called from under QSelectThread::mutex
void QSelectThread::cancelSelectOrIdle()
{
    if (maxSockfdInSelect < 0) {
        // terminate the idle or simple wait state
        cond.wakeOne();
    } else {
        // terminate select() execution
        ::so_cancel(maxSockfdInSelect);
    }
}

/*
  Returns true if the real time clock has changed by more than 10%
  relative to the processor time since the last time this function was
  called. This presumably means that the system time has been changed.

  If /a delta is nonzero, delta is set to our best guess at how much the system
  clock was changed.
*/
bool QSelectThread::timeChanged(timeval *delta)
{
    tms unused;
    clock_t currentTicks = times(&unused);

    int elapsedTicks = currentTicks - previousTicks;
    timeval elapsedTime = currentTime - previousTime;
    int elapsedMsecTicks = (elapsedTicks * 1000) / ticksPerSecond;
    int deltaMsecs = (elapsedTime.tv_sec * 1000 + elapsedTime.tv_usec / 1000)
                     - elapsedMsecTicks;

    if (delta) {
        delta->tv_sec = deltaMsecs / 1000;
        delta->tv_usec = (deltaMsecs % 1000) * 1000;
    }
    previousTicks = currentTicks;
    previousTime = currentTime;

    // If tick drift is more than 10% off compared to realtime, we assume that the clock has
    // been set. Of course, we have to allow for the tick granularity as well.

     return (qAbs(deltaMsecs) - msPerTick) * 10 > elapsedMsecTicks;
}

void QSelectThread::getTime(timeval &t)
{
    gettimeofday(&t, 0);
    // NTP-related fix
    while (t.tv_usec >= 1000000l) {
        t.tv_usec -= 1000000l;
        ++t.tv_sec;
    }
    while (t.tv_usec < 0l) {
        if (t.tv_sec > 0l) {
            t.tv_usec += 1000000l;
            --t.tv_sec;
        } else {
            t.tv_usec = 0l;
            break;
        }
    }
}

void QSelectThread::updateCurrentTime()
{
    getTime(currentTime);

    // repair timers if needed
    timeval delta;
    if (timeChanged(&delta))
        timerRepair(delta);
}

void QSelectThread::removeFromTimeoutMap(TimerInfo *ti)
{
    TimevalMap::iterator it = timersByTimeout.find(ti->timeout);
    while (it != timersByTimeout.end() && it.value() != ti)
        ++it;
    Q_ASSERT(it != timersByTimeout.end());
    timersByTimeout.erase(it);
}

/*
  repair broken timer
*/
void QSelectThread::timerRepair(const timeval &diff)
{
    // repair all timers
    timersByTimeout.clear();
    for (TimerInfoMap::iterator it = instance->timers.begin();
          it != instance->timers.end(); ++it) {
        register TimerInfo *t = it.value();
        t->timeout = t->timeout - diff;
        timersByTimeout.insertMulti(t->timeout, t);
    }
}

/*
  Returns the time to wait for the next timer, or false if no timers are
  waiting.
*/
bool QSelectThread::timerWait(timeval &tm)
{
    updateCurrentTime();

    if (timers.isEmpty())
        return false;

    // find first waiting timer with posted flag reset
    TimevalMap::iterator it = timersByTimeout.begin();
    while (it.value()->posted && it != timersByTimeout.end())
        ++it;
    if (it == timersByTimeout.end()) {
        // all timers've been posted but not yet processed by the target window;
        // set a flag to make sure the first getTimerObject(...,true) call will
        // interrupt the indefinte wait we are about to enter
        allTimersPosted = true;
        return false;
    }

    TimerInfo *t = it.value();
    if (currentTime < t->timeout) {
        // time to wait
        tm = t->timeout - currentTime;
    } else {
        // no time to wait
        tm.tv_sec  = 0;
        tm.tv_usec = 0;
    }

    return true;
}

/*
    Activates pending timers.
*/
void QSelectThread::activateTimers()
{
    if (timers.isEmpty())
        return; // nothing to do

    updateCurrentTime();

    TimevalMap::iterator it = timersByTimeout.begin();

    while (it != timersByTimeout.end()) {
        TimerInfo *t = it.value();

        if (currentTime < t->timeout)
            break; // no timer has expired

        if (t->posted) {
            // ignore the timer since we already posted it
            ++ it;
            continue;
        }

        // remove from map
        timersByTimeout.erase(it);

        // determine next timeout time after currentTime
        if (t->interval.tv_sec == 0 && t->interval.tv_usec == 0) {
            // zero timer
            t->timeout = currentTime;
        } else {
            register qint64 time = t->timeout.tv_sec * 1000ll + t->timeout.tv_usec / 1000ll;
            register qint64 curr = currentTime.tv_sec * 1000ll + currentTime.tv_usec / 1000ll;
            register qint64 ival = t->interval.tv_sec * 1000ll + t->interval.tv_usec / 1000ll;
            curr += (ival - ((curr - time) % ival));
            t->timeout.tv_sec = curr / 1000ll;
            t->timeout.tv_usec = (curr % 1000ll) * 1000ll;
        }

        // set the posted flag to avoid polluting the message queue with timer
        // messages if the target window is not fast enough to process them
        t->posted = true;

        // reinsert timer (in proper sort order)
        timersByTimeout.insertMulti(t->timeout, t);

        // post the timer message
        WinPostMsg(t->hwnd, WM_U_SEM_TIMER, MPFROMLONG(t->id), 0);

        // start over
        it = timersByTimeout.begin();
    }

    return;
}

#else

class QSelectThread
{
    // dummy implementation

public:
    static void addSelect(QSocketNotifier *notifier, HWND hwnd) {
#ifndef QT_NO_DEBUG
        qWarning("QSocketNotifier: socket notifiers require thread support but"
                 "QT_NO_THREAD was defined");
#endif
    }
    static void removeSelect(QSocketNotifier *notifier) {}
    static QSocketNotifier *getSocketNotifier(int key, bool reset); { return 0; }

    static void addTimer(int timerId, int interval, QObject *object, HWND hwnd) {
#ifndef QT_NO_DEBUG
        qWarning("QObject::startTimer: timers require thread support but"
                 "QT_NO_THREAD was defined");
#endif
    }
    static bool removeTimer(int timerId) { return false; }
    static bool removeTimers(QObject *object) { return false; }
    static QList<QPair<int, int> > knownTimers(QObject *object) const {
        return QList<QPair<int, int> >();
    }
    static QObject *getTimerObject(int timerId, bool reset);

    static void attachThread() {}
    static void detachThread() {}
};

#endif

class QEventDispatcherPMPrivate : public QAbstractEventDispatcherPrivate
{
    Q_DECLARE_PUBLIC(QEventDispatcherPM)
public:
    QEventDispatcherPMPrivate();
    ~QEventDispatcherPMPrivate();

    void createMsgQueue();
    void createAuxWnd();

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
        AuxWnd() : QPMObjectWindow(true /* deferred */) {}
        MRESULT message(ULONG msg, MPARAM mp1, MPARAM mp2);
    private:
        QSet<int> timersInSend;
    } auxWnd;

    HAB hab;
    HMQ hmq;

    bool interrupt;

    QList<QMSG> queuedUserInputEvents;
    QList<QMSG> queuedSocketEvents;
};

QEventDispatcherPMPrivate::QEventDispatcherPMPrivate()
    : hab(NULLHANDLE), hmq(NULLHANDLE), interrupt(false)
{
}

QEventDispatcherPMPrivate::~QEventDispatcherPMPrivate()
{
    auxWnd.destroy();
    if (hmq != NULLHANDLE) {
        WinDestroyMsgQueue(hmq);
        WinTerminate(hab);
    }
}

void QEventDispatcherPMPrivate::createMsgQueue()
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
            qWarning("QEventDispatcherPMPrivate::createMsgQueue: "
                     "WinCreateMsgQueue failed with 0x%08lX",
                     WinGetLastError(hab));
    }
}

void QEventDispatcherPMPrivate::createAuxWnd()
{
    if (auxWnd.hwnd() == NULLHANDLE) {
        createMsgQueue();
        auxWnd.create();
    }
}

MRESULT QEventDispatcherPMPrivate::AuxWnd::message(ULONG msg, MPARAM mp1, MPARAM mp2)
{
    QMSG qmsg = { hwnd(), msg, mp1, mp2 };

    QCoreApplication *app = QCoreApplication::instance();
    MRESULT result;
    if (app && app->filterEvent(&qmsg, reinterpret_cast<long *>(&result)))
        return result;

    switch (msg) {
        case WM_U_SEM_SELECT: {
            QSocketNotifier *notifier =
                QSelectThread::getSocketNotifier(LONGFROMMP(mp1), true /*reset*/);
            if (notifier) {
                QEvent event(QEvent::SockAct);
                QCoreApplication::sendEvent(notifier, &event);
            }
            break;
        }
        case WM_U_SEM_TIMER: {
            int timerId = LONGFROMMP(mp1);
            QObject *obj = QSelectThread::getTimerObject(timerId, true /*reset*/);

            if (obj && !timersInSend.contains(timerId)) {
                // send event, but don't allow it to recurse
                timersInSend += timerId;

                QTimerEvent e(timerId);
                QCoreApplication::sendEvent(obj, &e);

                timersInSend -= timerId;
            }
            break;
        }
        default:
            break;
    }

    return FALSE;
}

QEventDispatcherPM::QEventDispatcherPM(QObject *parent)
    : QAbstractEventDispatcher(*new QEventDispatcherPMPrivate, parent)
{
}

QEventDispatcherPM::~QEventDispatcherPM()
{
}

bool QEventDispatcherPM::processEvents(QEventLoop::ProcessEventsFlags flags)
{
    Q_D(QEventDispatcherPM);

    if (d->hmq == NULLHANDLE)
        d->createMsgQueue();

    d->interrupt = false;
    emit awake();

    bool canWait;
    bool retVal = false;

    QMSG waitMsg;
    bool haveWaitMsg = false;

    do {
        QCoreApplicationPrivate::sendPostedEvents(0, 0, d->threadData);

        while (!d->interrupt) {
            QMSG msg;
            bool haveMessage;

            if (!(flags & QEventLoop::ExcludeUserInputEvents) && !d->queuedUserInputEvents.isEmpty()) {
                // process queued user input events
                haveMessage = true;
                msg = d->queuedUserInputEvents.takeFirst();
            } else if(!(flags & QEventLoop::ExcludeSocketNotifiers) && !d->queuedSocketEvents.isEmpty()) {
                // process queued socket events
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
                    // queue user input events for later processing
                    haveMessage = false;
                    d->queuedUserInputEvents.append(msg);
                }
                if (haveMessage && (flags & QEventLoop::ExcludeSocketNotifiers)
                    && (msg.msg == WM_U_SEM_SELECT && msg.hwnd == d->auxWnd.hwnd())) {
                    // queue socket events for later processing
                    haveMessage = false;
                    d->queuedSocketEvents.append(msg);
                }
            }
            if (haveMessage) {
                if (msg.msg == WM_QUIT) {
                    if (QCoreApplication::instance()) {
                        if (QCoreApplication::instance()->d_func()->canQuit()) {
                            QCoreApplication::instance()->quit();
                            return true;
                        } else {
                            WinCancelShutdown(d->hmq, FALSE);
                            return true;
                        }
                    }
                    return false;
                }

                if (!filterEvent(&msg)) {
                    WinDispatchMsg(d->hab, &msg);
                }
            } else {
                // nothing todo so break
                break;
            }
            retVal = true;
        }

        // still nothing - wait for message
        QThreadData *data = d->threadData;
        canWait = (!retVal
                   && data->canWait
                   && !d->interrupt
                   && (flags & QEventLoop::WaitForMoreEvents));
        if (canWait) {
            emit aboutToBlock();
            WinGetMsg(d->hab, &waitMsg, 0, 0, 0);
            haveWaitMsg = true;
            emit awake();
        }
    } while (canWait);

    return retVal;
}

bool QEventDispatcherPM::hasPendingEvents()
{
    QMSG msg;
    return qGlobalPostedEventsCount() || WinPeekMsg(0, &msg, NULL, 0, 0, PM_NOREMOVE);
}

void QEventDispatcherPM::registerSocketNotifier(QSocketNotifier *notifier)
{
    Q_ASSERT(notifier);
#ifndef QT_NO_DEBUG
    int sockfd = notifier->socket();
    if (sockfd < 0
        || unsigned(sockfd) >= FD_SETSIZE) {
        qWarning("QSocketNotifier: Internal error");
        return;
    } else if (notifier->thread() != thread()
               || thread() != QThread::currentThread()) {
        qWarning("QSocketNotifier: socket notifiers cannot be enabled from another thread");
        return;
    }
#endif

    Q_D(QEventDispatcherPM);
    d->createAuxWnd();

    QSelectThread::addSelect(notifier, d->auxWnd.hwnd());
}

void QEventDispatcherPM::unregisterSocketNotifier(QSocketNotifier *notifier)
{
    Q_ASSERT(notifier);
#ifndef QT_NO_DEBUG
    int sockfd = notifier->socket();
    if (sockfd < 0
        || unsigned(sockfd) >= FD_SETSIZE) {
        qWarning("QSocketNotifier: Internal error");
        return;
    } else if (notifier->thread() != thread()
               || thread() != QThread::currentThread()) {
        qWarning("QSocketNotifier: socket notifiers cannot be disabled from another thread");
        return;
    }
#endif

    QSelectThread::removeSelect(notifier);
}

void QEventDispatcherPM::registerTimer(int timerId, int interval, QObject *object)
{
#ifndef QT_NO_DEBUG
    if (timerId < 1 || interval < 0 || !object) {
        qWarning("QEventDispatcherPM::registerTimer: invalid arguments");
        return;
    } else if (object->thread() != thread() || thread() != QThread::currentThread()) {
        qWarning("QObject::startTimer: timers cannot be started from another thread");
        return;
    }
#endif

    Q_D(QEventDispatcherPM);
    d->createAuxWnd();

    QSelectThread::addTimer(timerId, interval, object, d->auxWnd.hwnd());
}

bool QEventDispatcherPM::unregisterTimer(int timerId)
{
#ifndef QT_NO_DEBUG
    if (timerId < 1) {
        qWarning("QEventDispatcherPM::unregisterTimer: invalid argument");
        return false;
    } else if (thread() != QThread::currentThread()) {
        qWarning("QObject::killTimer: timers cannot be stopped from another thread");
        return false;
    }
#endif

    return QSelectThread::removeTimer(timerId);
}

bool QEventDispatcherPM::unregisterTimers(QObject *object)
{
#ifndef QT_NO_DEBUG
    if (!object) {
        qWarning("QEventDispatcherPM::unregisterTimers: invalid argument");
        return false;
    } else if (object->thread() != thread() || thread() != QThread::currentThread()) {
        qWarning("QObject::killTimers: timers cannot be stopped from another thread");
        return false;
    }
#endif

    return QSelectThread::removeTimers(object);
}

QList<QEventDispatcherPM::TimerInfo>
QEventDispatcherPM::registeredTimers(QObject *object) const
{
#ifndef QT_NO_DEBUG
    if (!object) {
        qWarning("QEventDispatcherPM:registeredTimers: invalid argument");
        return QList<TimerInfo>();
    }
#endif

    return QSelectThread::knownTimers(object);
}

void QEventDispatcherPM::wakeUp()
{
    Q_D(QEventDispatcherPM);
    WinPostQueueMsg(d->hmq, WM_NULL, 0, 0);
}

void QEventDispatcherPM::interrupt()
{
    Q_D(QEventDispatcherPM);
    d->interrupt = true;
    wakeUp();
}

void QEventDispatcherPM::flush()
{
}

void QEventDispatcherPM::startingUp()
{
    QSelectThread::attachThread();
}

void QEventDispatcherPM::closingDown()
{
    QSelectThread::detachThread();
}

void QEventDispatcherPM::createMsgQueue()
{
    Q_D(QEventDispatcherPM);
    if (d->hmq == NULLHANDLE)
        d->createMsgQueue();
}

QT_END_NAMESPACE
