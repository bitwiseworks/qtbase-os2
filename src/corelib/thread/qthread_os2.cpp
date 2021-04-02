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
#include "qthread.h"
#include "qthread_p.h"
#include "qthreadstorage.h"
#include "qmutex.h"

#include <private/qcoreapplication_p.h>
#include <private/qeventdispatcher_os2_p.h>

#ifndef QT_NO_THREAD

QT_BEGIN_NAMESPACE

static void qt_watch_adopted_thread(const int adoptedTID, QThread *qthread);
static void qt_adopted_thread_watcher_function(void *);

static QThreadData **qt_tls = NULL;

static void qt_create_tls()
{
    if (qt_tls != NULL)
        return;
    static QBasicMutex mutex;
    QMutexLocker locker(&mutex);
    APIRET arc = DosAllocThreadLocalMemory((sizeof(QThreadData *) + 3)/4,
                                           (PULONG *)&qt_tls);
    if (arc != NO_ERROR)
        qWarning("qt_create_tls: DosAllocThreadLocalMemory returned %lu", arc);
}

static void qt_free_tls()
{
    if (qt_tls != NULL) {
        DosFreeThreadLocalMemory((PULONG)qt_tls);
        qt_tls = NULL;
    }
}

Q_DESTRUCTOR_FUNCTION(qt_free_tls)

/*
    QThreadData
*/

void QThreadData::clearCurrentThreadData()
{
    *qt_tls = 0;
}

QThreadData *QThreadData::current(bool createIfNecessary)
{
    qt_create_tls();
    QThreadData *threadData = *qt_tls;
    if (!threadData && createIfNecessary) {
        threadData = new QThreadData;
        // This needs to be called prior to new AdoptedThread() to
        // avoid recursion.
        *qt_tls = threadData;
        threadData->thread = new QAdoptedThread(threadData);
        threadData->deref();
        threadData->isAdopted = true;
        threadData->threadId.storeRelaxed(reinterpret_cast<Qt::HANDLE>(_gettid()));

        if (!QCoreApplicationPrivate::theMainThread) {
            QCoreApplicationPrivate::theMainThread = threadData->thread.loadRelaxed();
        } else {
            qt_watch_adopted_thread(_gettid(), threadData->thread);
        }
    }
    return threadData;
}

void QAdoptedThread::init()
{
    d_func()->tid = _gettid();
}

static QVector<int> qt_adopted_thread_tids;
static QVector<QThread *> qt_adopted_qthreads;
static QBasicMutex qt_adopted_thread_watcher_mutex;
static int qt_adopted_thread_watcher_tid = 0;

/*!
    \internal
    Adds an adopted thread to the list of threads that Qt watches to make sure
    the thread data is properly cleaned up. This function starts the watcher
    thread if necessary.
*/
static void qt_watch_adopted_thread(const int adoptedTID, QThread *qthread)
{
    QMutexLocker lock(&qt_adopted_thread_watcher_mutex);

    if (_gettid() == qt_adopted_thread_watcher_tid) {
        return;
    }

    qt_adopted_thread_tids.append(adoptedTID);
    qt_adopted_qthreads.append(qthread);

    // Start watcher thread if it is not already running.
    if (qt_adopted_thread_watcher_tid == 0) {
        int tid = _beginthread(qt_adopted_thread_watcher_function, NULL, 0, NULL);
        if (tid == -1)
            qErrnoWarning(errno, "qt_watch_adopted_thread: _beginthread failed");
        else
            qt_adopted_thread_watcher_tid = tid;
    }
}

/*!
    \internal
    This function loops and waits for native adopted threads to finish.
    When this happens it derefs the QThreadData for the adopted thread
    to make sure it gets cleaned up properly.
*/
static void qt_adopted_thread_watcher_function(void *)
{
    QMutexLocker lock(&qt_adopted_thread_watcher_mutex);

    forever {
        if (qt_adopted_thread_tids.isEmpty()) {
            // our service is no longer necessary
            qt_adopted_thread_watcher_tid = 0;
            break;
        }

        lock.unlock();

        // Wait for any thread of this process
        APIRET arc;
        TID tid = 0;
        qDosNI(arc = DosWaitThread((PTID)&tid, DCWW_WAIT));

        lock.relock();

        if (arc == NO_ERROR) {
            int i = qt_adopted_thread_tids.indexOf (tid);
            if (i >= 0) {
                QThreadData *data = QThreadData::get2(qt_adopted_qthreads.at(i));

                lock.unlock();

                if (data->isAdopted) {
                    QThread *thread = data->thread;
                    Q_ASSERT(thread);
                    QThreadPrivate *thread_p = static_cast<QThreadPrivate *>(QObjectPrivate::get(thread));
                    Q_UNUSED(thread_p)
                    Q_ASSERT(!thread_p->finished);
                    thread_p->finish(thread);
                }
                data->deref();

                lock.relock();

                qt_adopted_thread_tids.remove(i);
                qt_adopted_qthreads.remove(i);
            }
        } else {
            // Should never fail.
            qWarning("qt_adopted_thread_watcher_function: DosWaitThread returned %lu", arc);
            break;
        }
    }

    QThreadData *threadData = *qt_tls;
    if (threadData)
        threadData->deref();
}

/**************************************************************************
 ** QThreadPrivate
 *************************************************************************/

#endif // QT_NO_THREAD

QAbstractEventDispatcher *QThreadPrivate::createEventDispatcher(QThreadData *data)
{
    Q_UNUSED(data);
    return new QEventDispatcherOS2;
}

#ifndef QT_NO_THREAD

// static
void QThreadPrivate::start(void *arg) noexcept
{
    QThread *thr = reinterpret_cast<QThread *>(arg);
    QThreadData *data = QThreadData::get2(thr);

    qt_create_tls();
    *qt_tls = data;
    data->threadId.storeRelaxed(reinterpret_cast<Qt::HANDLE>(_gettid()));

    QThread::setTerminationEnabled(false);

    {
        QMutexLocker locker(&thr->d_func()->mutex);
        data->quitNow = thr->d_func()->exited;
    }

    QAbstractEventDispatcher *eventDispatcher = data->eventDispatcher.loadRelaxed();
    if (!eventDispatcher) {
        eventDispatcher = createEventDispatcher(data);
        data->eventDispatcher.storeRelease(eventDispatcher);
    }

    eventDispatcher->startingUp();

    emit thr->started(QThread::QPrivateSignal());
    QThread::setTerminationEnabled(true);
    thr->run();

    finish(arg);
}

// static
void QThreadPrivate::finish(void *arg, bool lockAnyway) noexcept
{
    QThread *thr = reinterpret_cast<QThread *>(arg);
    QThreadPrivate *d = thr->d_func();

    QMutexLocker locker(lockAnyway ? &d->mutex : 0);
    d->isInFinish = true;
    d->priority = QThread::InheritPriority;
    void **tls_data = reinterpret_cast<void **>(&d->data->tls);
    locker.unlock();
    emit thr->finished(QThread::QPrivateSignal());
    QCoreApplication::sendPostedEvents(0, QEvent::DeferredDelete);
    QThreadStorageData::finish(tls_data);
    locker.relock();

    QAbstractEventDispatcher *eventDispatcher = d->data->eventDispatcher.loadRelaxed();
    if (eventDispatcher) {
        d->data->eventDispatcher = 0;
        locker.unlock();
        eventDispatcher->closingDown();
        delete eventDispatcher;
        locker.relock();
    }

    d->running = false;
    d->finished = true;
    d->isInFinish = false;
    d->interruptionRequested = false;

    d->tid = 0;
}

/**************************************************************************
 ** QThread
 *************************************************************************/

Qt::HANDLE QThread::currentThreadId() Q_DECL_NOTHROW
{
    return reinterpret_cast<Qt::HANDLE>(_gettid());
}

int QThread::idealThreadCount() Q_DECL_NOTHROW
{
    ULONG cpuCnt = 1;
    APIRET rc = DosQuerySysInfo(QSV_NUMPROCESSORS, QSV_NUMPROCESSORS,
                                &cpuCnt, sizeof(cpuCnt));
    if (rc != NO_ERROR || cpuCnt == 0)
        cpuCnt = 1;
    return (int) cpuCnt;
}

void QThread::yieldCurrentThread()
{
    DosSleep(0);
}

void QThread::sleep(unsigned long secs)
{
    DosSleep(secs * 1000);
}

void QThread::msleep(unsigned long msecs)
{
    DosSleep(msecs);
}

void QThread::usleep(unsigned long usecs)
{
    DosSleep((usecs / 1000) + 1);
}

// NOTE: Caller must hold the mutex.
void QThreadPrivate::setPriority(QThread::Priority priority)
{
    this->priority = priority;

    ULONG prioClass = 0;
    LONG prioDelta = 0;
    switch (priority) {
        case QThread::IdlePriority:
            prioClass = PRTYC_IDLETIME;
            prioDelta = PRTYD_MINIMUM;
            break;

        case QThread::LowestPriority:
            prioClass = PRTYC_IDLETIME;
            break;

        case QThread::LowPriority:
            prioClass = PRTYC_IDLETIME;
            prioDelta = PRTYD_MAXIMUM;
            break;

        case QThread::NormalPriority:
        	prioClass = PRTYC_REGULAR;
        	break;

        case QThread::HighPriority:
            prioClass = PRTYC_REGULAR;
            prioDelta = PRTYD_MAXIMUM;
            break;

        case QThread::HighestPriority:
        	prioClass = PRTYC_TIMECRITICAL;
        	break;

        case QThread::TimeCriticalPriority:
            prioClass = PRTYC_TIMECRITICAL;
            prioDelta = PRTYD_MAXIMUM;
            break;

        case QThread::InheritPriority:
        default:
            PTIB ptib;
            DosGetInfoBlocks(&ptib, NULL);
            prioClass = (ptib->tib_ptib2->tib2_ulpri >> 8) & 0xFF;
            prioDelta = (ptib->tib_ptib2->tib2_ulpri) & 0xFF;
            break;
    }

    APIRET rc = DosSetPriority(PRTYS_THREAD, prioClass, prioDelta, tid);
    if (rc != NO_ERROR)
        qWarning("QThreadPrivate::setPriority: DosSetPriority returned %lu", rc);
}

void QThread::start(Priority priority)
{
    Q_D(QThread);
    QMutexLocker locker(&d->mutex);

    if (d->isInFinish) {
        locker.unlock();
        wait();
        locker.relock();
    }

    if (d->running)
        return;

    d->running = true;
    d->finished = false;
    d->exited = false;
    d->returnCode = 0;
    d->interruptionRequested = false;

    int tid = _beginthread(QThreadPrivate::start, NULL, d->stackSize, this);
    d->tid = tid != -1 ? (TID) tid : 0;

    if (tid == -1) {
        qErrnoWarning(errno, "QThread::start: Failed to create thread");
        d->running = false;
        d->finished = true;
        return;
    }

    d->setPriority(priority);
}

void QThread::terminate()
{
    Q_D(QThread);
    QMutexLocker locker(&d->mutex);
    if (d->tid == (TID)QThread::currentThreadId()) {
        qWarning("QThread::terminate: thread cannot terminate itself" );
        return;
    }
    if (!d->running)
        return;
    if (!d->terminationEnabled) {
        d->terminatePending = true;
        return;
    }
    DosKillThread(d->tid);
    QThreadPrivate::finish(this, false);
}

bool QThread::wait(QDeadlineTimer deadline)
{
    Q_D(QThread);
    QMutexLocker locker(&d->mutex);

    if (d->tid == (TID)QThread::currentThreadId()) {
        qWarning("QThread::wait: Thread tried to wait on itself");
        return false;
    }
    if (d->finished || !d->running)
        return true;

    locker.mutex()->unlock();

    unsigned long time = deadline.remainingTime();
    bool ret = true;
    TID tid = d->tid;
    APIRET rc = NO_ERROR;
    if (time == ULONG_MAX) {
        qDosNI(rc = DosWaitThread(&tid, DCWW_WAIT));
    } else {
        ULONG sleep;
        do {
            // OS/2 doesn't support specifying a maximum time to wait for a
            // thread to end other than infininty, so we run a loop of small
            // wait intervals (in ms)
            constexpr ULONG THREAD_WAIT_INTERVAL = 1000;
            sleep = time > THREAD_WAIT_INTERVAL ? THREAD_WAIT_INTERVAL : time;
            DosSleep(sleep);
            qDosNI(rc = DosWaitThread(&tid, DCWW_NOWAIT));
            if (rc != ERROR_THREAD_NOT_TERMINATED)
                break;
            if (d->finished || !d->running) {
                rc = 0;
                break;
            }
            time -= sleep;
        } while (time);
    }
    if (rc != NO_ERROR && rc != ERROR_INVALID_THREADID) {
        if (rc != ERROR_THREAD_NOT_TERMINATED)
            qWarning("QThread::wait: DosWaitThread returned %lu", rc);
        ret = false;
    }

    locker.mutex()->lock();

    if (ret && !d->finished) {
        // thread was terminated by someone else
        QThreadPrivate::finish(this, false);
    }

    return ret;
}

void QThread::setTerminationEnabled(bool enabled)
{
    QThread *thr = currentThread();
    Q_ASSERT_X(thr != 0, "QThread::setTerminationEnabled()",
               "Current thread was not started with QThread.");
    QThreadPrivate *d = thr->d_func();
    QMutexLocker locker(&d->mutex);
    d->terminationEnabled = enabled;
    if (enabled && d->terminatePending) {
        QThreadPrivate::finish(thr, false);
        locker.unlock(); // don't leave the mutex locked!
        _endthread();
    }
}

QT_END_NAMESPACE

#endif // QT_NO_THREAD
