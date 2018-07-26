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

#include "qthread.h"
#include "qthread_p.h"
#include "qthreadstorage.h"
#include "qmutex.h"

#include <qcoreapplication.h>
#include <qpointer.h>

#include <private/qcoreapplication_p.h>
#include <private/qeventdispatcher_pm_p.h>

#include <qt_os2.h>

#ifndef QT_NO_THREAD

QT_BEGIN_NAMESPACE

static void qt_watch_adopted_thread(const TID adoptedTID, QThread *qthread);
static void qt_adopted_thread_watcher_function(void *);

static QThreadData **qt_tls = NULL;

Q_CORE_EXPORT void qt_create_tls()
{
    if (qt_tls != NULL)
        return;
    static QMutex mutex;
    QMutexLocker locker(&mutex);
    APIRET rc = DosAllocThreadLocalMemory((sizeof(QThreadData *) + 3)/4,
                                          (PULONG *)&qt_tls);
    if (rc != NO_ERROR)
        qWarning("qt_create_tls: DosAllocThreadLocalMemory returned %lu", rc);
}

static void qt_free_tls()
{
    if (qt_tls != NULL) {
        DosFreeThreadLocalMemory((PULONG)qt_tls);
        qt_tls = NULL;
    }
}

static Q_DESTRUCTOR_FUNCTION(qt_free_tls)

/*
    QThreadData
*/
QThreadData *QThreadData::current()
{
    qt_create_tls();
    QThreadData *threadData = *qt_tls;
    if (!threadData) {
        QThread *adopted = 0;
        if (QInternal::activateCallbacks(QInternal::AdoptCurrentThread, (void **) &adopted)) {
            Q_ASSERT(adopted);
            threadData = QThreadData::get2(adopted);
            *qt_tls = threadData;
            adopted->d_func()->running = true;
            adopted->d_func()->finished = false;
            static_cast<QAdoptedThread *>(adopted)->init();
        } else {
            threadData = new QThreadData;
            // This needs to be called prior to new AdoptedThread() to
            // avoid recursion.
            *qt_tls = threadData;
            threadData->thread = new QAdoptedThread(threadData);
            threadData->deref();
        }

        if (!QCoreApplicationPrivate::theMainThread) {
            QCoreApplicationPrivate::theMainThread = threadData->thread;
        } else {
            PTIB ptib;
            DosGetInfoBlocks(&ptib, NULL);
            qt_watch_adopted_thread(ptib->tib_ptib2->tib2_ultid, threadData->thread);
        }
    }
    return threadData;
}

void QAdoptedThread::init()
{
    PTIB ptib;
    DosGetInfoBlocks(&ptib, NULL);
    d_func()->tid = ptib->tib_ptib2->tib2_ultid;
}

static QVector<TID> qt_adopted_thread_tids;
static QVector<QThread *> qt_adopted_qthreads;
static QMutex qt_adopted_thread_watcher_mutex;
static TID qt_adopted_thread_watcher_tid = 0;
static HEV qt_adopted_thread_wakeup = NULLHANDLE;

/*! \internal
    Adds an adopted thread to the list of threads that Qt watches to make sure
    the thread data is properly cleaned up. This function starts the watcher
    thread if necessary.
*/
static void qt_watch_adopted_thread(const TID adoptedTID, QThread *qthread)
{
    QMutexLocker lock(&qt_adopted_thread_watcher_mutex);
    qt_adopted_thread_tids.append(adoptedTID);
    qt_adopted_qthreads.append(qthread);

    // Start watcher thread if it is not already running.
    if (qt_adopted_thread_watcher_tid == 0) {
        APIRET rc = DosCreateEventSem(NULL, &qt_adopted_thread_wakeup,
                                      DCE_AUTORESET, FALSE);
        if (rc != NO_ERROR) {
            qWarning("qt_watch_adopted_thread: DosCreateEventSem returned %lu", rc);
            return;
        }

        int tid =
            (TID)_beginthread(qt_adopted_thread_watcher_function, NULL, 0, NULL);
        if (tid == -1)
            qErrnoWarning(errno, "qt_watch_adopted_thread: _beginthread failed");
        else
            qt_adopted_thread_watcher_tid = (TID) tid;
    } else {
        DosPostEventSem(qt_adopted_thread_wakeup);
    }
}

/*! \internal
    This function loops and waits for native adopted threads to finish.
    When this happens it derefs the QThreadData for the adopted thread
    to make sure it gets cleaned up properly.
*/
static void qt_adopted_thread_watcher_function(void *)
{
    forever {
        qt_adopted_thread_watcher_mutex.lock();

        if (qt_adopted_thread_tids.isEmpty()) {
            // our service is no longer necessary
            qt_adopted_thread_watcher_tid = 0;
            DosCloseEventSem(qt_adopted_thread_wakeup);
            qt_adopted_thread_wakeup = NULLHANDLE;
            qt_adopted_thread_watcher_mutex.unlock();
            break;
        }

        APIRET rc;
        TID tid;

        for (int i = 0; i < qt_adopted_thread_tids.size();) {
            tid = qt_adopted_thread_tids.at(i);
            qDosNI(rc = DosWaitThread(&tid, DCWW_NOWAIT));
            if (rc != ERROR_THREAD_NOT_TERMINATED) {
                if (rc == NO_ERROR || rc == ERROR_INVALID_THREADID) {
                    QThreadData::get2(qt_adopted_qthreads.at(i))->deref();
                    qt_adopted_thread_tids.remove(i);
                    qt_adopted_qthreads.remove(i);
                    continue;
                }
                qWarning("qt_adopted_thread_watcher_function: DosWaitThread returned %lu", rc);
            }
            ++i;
        }

        qt_adopted_thread_watcher_mutex.unlock();

        qDosNI(rc = DosWaitEventSem(qt_adopted_thread_wakeup, 300));
        if (rc != NO_ERROR && rc != ERROR_TIMEOUT) {
            qWarning("qt_adopted_thread_watcher_function: DosWaitEventSem returned %lu", rc);
        }
    }
}

/**************************************************************************
 ** QThreadPrivate
 *************************************************************************/

#endif // QT_NO_THREAD

void QThreadPrivate::createEventDispatcher(QThreadData *data)
{
    data->eventDispatcher = new QEventDispatcherPM;
    data->eventDispatcher->startingUp();
}

#ifndef QT_NO_THREAD

// static
void QThreadPrivate::start(void *arg)
{
    QThread *thr = reinterpret_cast<QThread *>(arg);
    QThreadData *data = QThreadData::get2(thr);

    qt_create_tls();
    *qt_tls = data;

    QThread::setTerminationEnabled(false);

    data->quitNow = false;
    // ### TODO: allow the user to create a custom event dispatcher
    if (QCoreApplication::instance())
        createEventDispatcher(data);

    emit thr->started();
    QThread::setTerminationEnabled(true);
    thr->run();

    finish(arg);
}

// static
void QThreadPrivate::finish(void *arg, bool lockAnyway)
{
    QThread *thr = reinterpret_cast<QThread *>(arg);
    QThreadPrivate *d = thr->d_func();

    if (lockAnyway)
        d->mutex.lock();
    d->priority = QThread::InheritPriority;
    d->running = false;
    d->finished = true;
    if (d->terminated)
        emit thr->terminated();
    d->terminated = false;
    emit thr->finished();

    if (d->data->eventDispatcher) {
        d->data->eventDispatcher->closingDown();
        QAbstractEventDispatcher *eventDispatcher = d->data->eventDispatcher;
        d->data->eventDispatcher = 0;
        delete eventDispatcher;
    }

    QThreadStorageData::finish(reinterpret_cast<void **>(&d->data->tls));

    d->tid = 0;

    if (lockAnyway)
        d->mutex.unlock();
}

/**************************************************************************
 ** QThread
 *************************************************************************/

Qt::HANDLE QThread::currentThreadId()
{
    PTIB ptib;
    DosGetInfoBlocks(&ptib, NULL);
    return (Qt::HANDLE)ptib->tib_ptib2->tib2_ultid;
}

int QThread::idealThreadCount()
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

    if (d->running)
        return;

    d->running = true;
    d->finished = false;
    d->terminated = false;

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
    d->terminated = true;
    QThreadPrivate::finish(this, false);
}

bool QThread::wait(unsigned long time)
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
            enum { THREAD_WAIT_INTERVAL = 1000 };
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
        d->terminated = true;
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
        d->terminated = true;
        QThreadPrivate::finish(thr, false);
        locker.unlock(); // don't leave the mutex locked!
        _endthread();
    }
}

void QThread::setPriority(Priority priority)
{
    Q_D(QThread);
    QMutexLocker locker(&d->mutex);
    if (!d->running) {
        qWarning("QThread::setPriority: Cannot set priority, thread is not running");
        return;
    }

    d->setPriority(priority);
}

QT_END_NAMESPACE

#endif // QT_NO_THREAD
