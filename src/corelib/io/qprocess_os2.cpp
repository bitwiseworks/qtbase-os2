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

//#define QPROCESS_DEBUG

#include "qplatformdefs.h"

#include <emx/umalloc.h> // for _lmalloc()

#include <libcx/spawn2.h>

#if defined QPROCESS_DEBUG
#include "qdebug.h"
#include "qstring.h"
#include <ctype.h>

#include <qthread.h>

QT_BEGIN_NAMESPACE

// defined in qprocess.cpp
extern QByteArray qt_prettyDebug(const char *data, int len, int maxSize);

// redirect qDebug output to a file in the root directory of the current drive
static void msgHandler(QtMsgType, const QMessageLogContext &ctx, const QString &msg)
{
    static FILE *f = 0;
    if (!f) {
        f = fopen("\\qprocess.dbg", "wb");
        setbuf(f, NULL);
    }
    fprintf(f, "%s:%d: %s\n", ctx.function, ctx.line, msg.toLocal8Bit ().data ());
}

QT_END_NAMESPACE

#define DEBUG(a) qDebug a
#else
#define DEBUG(a) do {} while(0)
#endif

#include "qprocess.h"
#include "qprocess_p.h"
#include <qglobal.h>

#include <private/qcoreapplication_p.h>
#include <private/qthread_p.h>
#include <private/qsystemerror_p.h>
#include <qdatetime.h>
#include <qfile.h>
#include <qfileinfo.h>
#include <qhash.h>
#include <qmutex.h>
#include <qdir.h>

#define HF_STDIN        HFILE(0)
#define HF_STDOUT       HFILE(1)
#define HF_STDERR       HFILE(2)

QT_BEGIN_NAMESPACE

// defined in qcoreapplication.cpp
extern QString qAppFileName();

#if QT_CONFIG(process)

// NOTE: Copied over from qprocess_unix.cpp.
QProcessEnvironment QProcessEnvironment::systemEnvironment()
{
    QProcessEnvironment env;
    const char *entry;
    for (int count = 0; (entry = environ[count]); ++count) {
        const char *equal = strchr(entry, '=');
        if (!equal)
            continue;

        QByteArray name(entry, equal - entry);
        QByteArray value(equal + 1);
        env.d->vars.insert(QProcessEnvironmentPrivate::Key(name),
                           QProcessEnvironmentPrivate::Value(value));
    }
    return env;
}

enum
{
    PIPE_SIZE_STDIN = 65512, // max (at least on eCS)
    PIPE_SIZE_STDOUT = 65512, // max (at least on eCS)
    PIPE_SIZE_STDERR = 4096,
};

class QProcessManager : public QThread
{
public:

    enum { InvalidProcKey = USHORT(~0), MaxProcKey = (USHORT(~0) >> 2) };

    static void addRef();
    static void release();

    static USHORT addProcess(QProcess *process);
    static void removeProcess(USHORT procKey);

    static QMutex *pipeStateLock() {
        Q_ASSERT(instance);
        return &instance->pipeStatLock;
    }

private:

    QProcessManager();
    ~QProcessManager();

    void installSigHandler();
    void uninstallSigHandler();
    void run();

    int refcnt;
    bool finish;

    HEV eventSem;
    QAtomicInt eventSemGuard;
    QAtomicInt deathFlag;
    QMutex pipeStatLock;

    typedef QHash<USHORT, QProcess *> ProcessList;
    ProcessList processes;
    USHORT lastProcKey;

    void (*sa_old_sigchld_handler)(int);

    static void sa_sigchld_handler(int signum);

    static USHORT toPipeKey(USHORT procKey, QProcessPrivate::PipeType type) {
        Q_ASSERT((procKey << 2) >> 2 == procKey);
        return (procKey << 2) | type;
    }
    static USHORT toProcKey(USHORT key) { return key >> 2; }
    static QProcessPrivate::PipeType toPipeType(USHORT key) {
        return QProcessPrivate::PipeType(key & 0x3);
    }

    static QProcessManager *instance;
    static QMutex mutex;
};

// static
QProcessManager *QProcessManager::instance = 0;
QMutex QProcessManager::mutex;

// static
void QProcessManager::sa_sigchld_handler(int signum)
{
#if defined (QPROCESS_DEBUG)
    //fprintf(stderr, "*** SIGCHLD\n");
#endif

    Q_ASSERT(instance);

    // eventSemGuard is used as follows:
    // 0 - QProcessManager is not operational
    // 1 - QProcessManager is being run, eventSem is fine
    // 2 - another signal handler is in action

    if (!instance->eventSemGuard.testAndSetAcquire(1, 2))
        return;

    // set deathFlag to 1 and post the semaphore if not already done so
    if (instance->deathFlag.testAndSetRelaxed(0, 1)) {
        APIRET rc = DosPostEventSem(instance->eventSem);
        Q_ASSERT(rc == NO_ERROR || rc == ERROR_ALREADY_POSTED);
        Q_UNUSED(rc);
    }

    instance->eventSemGuard.testAndSetRelease(2, 1);

    if (instance->sa_old_sigchld_handler &&
        instance->sa_old_sigchld_handler != SIG_IGN)
        instance->sa_old_sigchld_handler(signum);
}

// static
void QProcessManager::addRef()
{
    QMutexLocker locker(&mutex);

    if (instance == 0) {
        instance = new QProcessManager();
    }

    ++instance->refcnt;
}

// static
void QProcessManager::release()
{
    QMutexLocker locker(&mutex);

    Q_ASSERT(instance);

    if (--instance->refcnt == 0) {
        // make sure we don't globally exist anymore before leaving the lock
        QProcessManager *instance = QProcessManager::instance;
        QProcessManager::instance = 0;
        // disable the signal handler and stop the thread if necessary
        if (instance->isRunning()) {
            Q_ASSERT(instance->eventSemGuard == 1 || instance->eventSemGuard == 2);
            while (!instance->eventSemGuard.testAndSetRelease(1, 0))
                DosSleep(0);
            instance->uninstallSigHandler();
            // stop the thread
            instance->finish = true;
            locker.unlock();
            DosPostEventSem(instance->eventSem);
            instance->wait();
        }
        delete instance;
    }
}

// static
USHORT QProcessManager::addProcess(QProcess *process)
{
    DEBUG(("%p", process));

    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    // lazily enable SIGCHLD handler and start the worker
    if (instance->eventSemGuard.testAndSetAcquire(0, 1)) {
        instance->installSigHandler();
        instance->start();
    }

    QProcessPrivate *d = process->d_func();

    USHORT procKey = instance->lastProcKey + 1;
    if (procKey > MaxProcKey) {
        // limit reached, find an unused number
        procKey = 0;
        while (++procKey <= MaxProcKey &&
               instance->processes.contains(procKey));
        Q_ASSERT(procKey <= MaxProcKey);
        if (procKey > MaxProcKey) {
            // oops, no more free keys!
            locker.unlock();
            d->setErrorAndEmit(QProcess::FailedToStart,
                               QProcess::tr("Resource error: Too many processes"));
            return InvalidProcKey;
        }
    } else {
        instance->lastProcKey = procKey;
    }

    // attach the semahpore to the pipes of the process
    APIRET arc = NO_ERROR;
    if (d->stdinChannel.type == QProcessPrivate::Channel::Normal &&
        d->stdinChannel.pipe.server != INVALID_HFILE) {
        arc = DosSetNPipeSem(d->stdinChannel.pipe.server, (HSEM)instance->eventSem,
                             toPipeKey(procKey, QProcessPrivate::InPipe));
    }
    if (arc == NO_ERROR &&
        d->stdoutChannel.type == QProcessPrivate::Channel::Normal &&
        d->stdoutChannel.pipe.server != INVALID_HFILE) {
        arc = DosSetNPipeSem(d->stdoutChannel.pipe.server, (HSEM)instance->eventSem,
                             toPipeKey(procKey, QProcessPrivate::OutPipe));
    }
    if (arc == NO_ERROR &&
        d->stderrChannel.type == QProcessPrivate::Channel::Normal &&
        d->stderrChannel.pipe.server != INVALID_HFILE) {
        arc = DosSetNPipeSem(d->stderrChannel.pipe.server, (HSEM)instance->eventSem,
                             toPipeKey(procKey, QProcessPrivate::ErrPipe));
    }
    if (arc != NO_ERROR) {
        if (procKey == instance->lastProcKey)
            --instance->lastProcKey;
        locker.unlock();
        d->setErrorAndEmit(QProcess::FailedToStart,
                           QProcess::tr("Resource error: %1").arg(QSystemError::os2String(arc)));
        return InvalidProcKey;
    }

    instance->processes[procKey] = process;

    return procKey;
}

// static
void QProcessManager::removeProcess(USHORT procKey)
{
    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    Q_ASSERT(instance->processes.contains(procKey));
    QProcess *process = instance->processes[procKey];

    DEBUG(("%p", process));

    // to guarantee that the given procKey may be reused, we must close all
    // pipes in order to ensure that we won't get late NPSS_CLOSE for the
    // removed process with the key that may be already associated with a new one
    QProcessPrivate *d = process->d_func();
    d->closeChannel(&d->stdinChannel);
    d->closeChannel(&d->stdoutChannel);
    d->closeChannel(&d->stderrChannel);

    instance->processes.remove(procKey);

    // small optimization: released the highest key
    if (procKey == instance->lastProcKey)
        --instance->lastProcKey;
}

QProcessManager::QProcessManager()
    : refcnt(0), finish(false), eventSem(NULLHANDLE), sa_old_sigchld_handler(0)
{
    DEBUG(());

    APIRET rc = DosCreateEventSem(NULL, &eventSem,
                                  DC_SEM_SHARED | DCE_AUTORESET | DCE_POSTONE,
                                  FALSE);
    Q_ASSERT(rc == NO_ERROR);
    Q_UNUSED(rc);

    lastProcKey = InvalidProcKey;
}

QProcessManager::~QProcessManager()
{
    DEBUG(());

    Q_ASSERT(!refcnt);
    Q_ASSERT(!processes.size());

    DosCloseEventSem(eventSem);
}

void QProcessManager::installSigHandler()
{
    // set up the SIGCHLD handler which posts a semaphore whenever
    // our child dies
    struct sigaction oldAction;
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_handler = sa_sigchld_handler;
    action.sa_flags = SA_NOCLDSTOP;
    ::sigaction(SIGCHLD, &action, &oldAction);
    if (oldAction.sa_handler != sa_sigchld_handler)
    sa_old_sigchld_handler = oldAction.sa_handler;
}

void QProcessManager::uninstallSigHandler()
{
    struct sigaction oldAction;
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_handler = sa_old_sigchld_handler;
    action.sa_flags = SA_NOCLDSTOP;
    ::sigaction(SIGCHLD, &action, &oldAction);
    if (oldAction.sa_handler != sa_sigchld_handler) {
        ::sigaction(SIGCHLD, &oldAction, 0);
    }
}

void QProcessManager::run()
{
    DEBUG(() << "BEGIN");

    // Note: the rationale behind using a worker thread for death detection is
    // that calling complex functions from a signal handler is not really a good
    // idea unless there is a 100% guarantee that they are reentrant. So, the
    // handler only posts a semaphore (I *hope* DosPostEventSem is reentrant)
    // and all complex work is done here asynchronously.

    QMutexLocker locker(&mutex);
    APIRET arc;

    // an array for 1 process is initially enough
    int pipeStatesSize = sizeof(PIPESEMSTATE) * (3 * 2 + 1);
    PPIPESEMSTATE pipeStates = (PPIPESEMSTATE)::_lmalloc(pipeStatesSize);

    do {
        locker.unlock();
        qDosNI(arc = DosWaitEventSem(eventSem, SEM_INDEFINITE_WAIT));
        locker.relock();

        if (finish)
            break;

        if (instance->deathFlag.testAndSetRelaxed(1, 0)) {
            DEBUG(() << "child death signaled");
            for (QProcess *proc : processes) {
                DEBUG(() << "pid" << proc->d_func()->pid << "waitMode" << hex << proc->d_func()->waitMode);
                if (proc->d_func()->waitMode) {
                    DosPostEventSem(proc->d_func()->waitSem);
                } else {
                    QMetaObject::invokeMethod(proc, "_q_notified", Qt::QueuedConnection,
                                              Q_ARG(int, QProcessPrivate::CanDie));
                }
            }
        }

        // we have no running processes, start over (note that QProcessPrivate
        // will release us on destruction and this will cause thread termination)
        if (processes.size() == 0)
            continue;

        DEBUG(() << "checking pipes");

        // make sure the state array is big enough. The best size for sz pipes
        // is sz * 2 (to be able to store both NPSS_RDATA/NPSS_WSPACE and
        // NPSS_CLOSE for every pipe) + one for NPSS_EOI
        int bestSize = sizeof(PIPESEMSTATE) * (processes.size() * 3 * 2 + 1);
        if (pipeStatesSize < bestSize) {
            pipeStates = (PPIPESEMSTATE)_lrealloc(pipeStates, bestSize);
            pipeStatesSize = bestSize;
        }

        // Note: OS/2 posts the semaphore once for each new pipe event but
        // DosQueryNPipeSemState() returns all previous events (already posted
        // on their own) that are still up to date. This makes it impossible to
        // distinguish between the repeated events and unique ones here. This is
        // done in _q_notified() using some heuristic. Due to the same reason of
        // duplicate events we do a lock here to be in sync with writeToStdin().
        // Note that all this hassle would not be necessary at all if there were
        // a method to get the number of free bytes in the write buf of the pipe
        // (similar to DosPeekNPipe() that checks the read buf) but there isn't.

        QMutexLocker lock(pipeStateLock());

        arc = DosQueryNPipeSemState((HSEM)eventSem, pipeStates, pipeStatesSize);
        if (arc != NO_ERROR) {
            qWarning("QProcessManager::run: DosQueryNPipeSemState returned %lu", arc);
            continue;
        }

        // In the returned information array, CLOSE and READ records for the
        // same pipe key may be mixed. We need CLOSE messages to be posted after
        // READ messages, so we do two passes.

        // @todo We don't need two passes any more!

        int pass = 0;
        for (int i = 0; pass < 2; ++i) {
            BYTE status = pipeStates[i].fStatus;
            if (status == NPSS_EOI) {
                ++ pass;
                i = -1;
                continue;
            }
            if (pass == 0 && status != NPSS_RDATA && status != NPSS_WSPACE)
                continue;
            if (pass == 1 && status != NPSS_CLOSE)
                continue;
            DEBUG((" %d/%d: fStatus %u fFlag %02X usKey %04hX usAvail %hu",
                   pass, i, (uint) pipeStates[i].fStatus,
                   (uint) pipeStates[i].fFlag, pipeStates[i].usKey,
                   pipeStates[i].usAvail));
            int procKey = toProcKey(pipeStates[i].usKey);
            QProcessPrivate::PipeType type = toPipeType(pipeStates[i].usKey);

            QProcess *proc = processes[procKey];
            Q_ASSERT(proc);
            DEBUG(("  process %p pid %d type %d", proc, proc->d_func()->pid, type));

            int flags = 0;
            switch(type) {
            case QProcessPrivate::InPipe:
                Q_ASSERT(status == NPSS_CLOSE || status == NPSS_WSPACE);
                flags |= QProcessPrivate::CanWrite;
                // save the current number of free bytes in the pipe to let
                // _q_canWrite() go (this is also used in tryCloseStdinPipe())
                proc->d_func()->pipes[type]->bytes = pipeStates[i].usAvail;
                break;
            case QProcessPrivate::OutPipe:
                Q_ASSERT(status == NPSS_CLOSE || status == NPSS_RDATA);
                flags |= QProcessPrivate::CanReadStdOut;
                break;
            case QProcessPrivate::ErrPipe:
                Q_ASSERT(status == NPSS_CLOSE || status == NPSS_RDATA);
                flags |= QProcessPrivate::CanReadStdErr;
                break;
            }

            if (proc->d_func()->waitMode) {
                DosPostEventSem(proc->d_func()->waitSem);
            } else {
                QMetaObject::invokeMethod(proc, "_q_notified", Qt::QueuedConnection,
                                          Q_ARG(int, flags));
            }
        }
    } while (true);

    ::free(pipeStates);

    DEBUG(() << "END");
}


void QProcessPrivate::init()
{
    waitMode = 0;
    waitSem = NULLHANDLE;

    procKey = QProcessManager::InvalidProcKey;

    threadSafe = false;

    QProcessManager::addRef();
}

void QProcessPrivate::uninit()
{
    QProcessManager::release();

    if (waitSem != NULLHANDLE)
        DosCloseEventSem(waitSem);
}

void QProcessPrivate::ensureWaitSem()
{
    if (waitSem == NULLHANDLE) {
        APIRET rc = DosCreateEventSem(0, &waitSem, DCE_AUTORESET, FALSE);
        Q_ASSERT(rc == NO_ERROR);
        Q_UNUSED(rc);
    }
}

bool QProcessPrivate::createPipe(PipeType type, Channel::Pipe &pipe,
                                 const char *name /*= 0*/)
{
    APIRET arc = NO_ERROR;
    char pathBuf[CCHMAXPATH];

    // we need the process identifier to guarantee pipe name unicity
    PPIB ppib = NULL;
    DosGetInfoBlocks(NULL, &ppib);

    switch (type) {
    case InPipe:
        // create our end of the pipe
        sprintf(pathBuf, "\\pipe\\Qt5\\%08lX\\QProcess\\%p\\%s",
                ppib->pib_ulpid, this->q_func(), name ? name : "Stdin");
        arc = DosCreateNPipe(pathBuf, &pipe.server,
                             NP_ACCESS_OUTBOUND | NP_NOINHERIT,
                             NP_NOWAIT | NP_TYPE_BYTE | 1,
                             PIPE_SIZE_STDIN, 0, 0);
        if (arc == NO_ERROR) {
            DosConnectNPipe(pipe.server);
            // ensure the other end blocks (vital for process->process redirections)
            DosSetNPHState(pipe.server, NP_WAIT);
            // open the client end of the pipe
            ULONG action = 0;
            arc = DosOpen(pathBuf, &pipe.client, &action, 0, FILE_NORMAL, FILE_OPEN,
                          OPEN_ACCESS_READONLY | OPEN_SHARE_DENYREADWRITE |
                          OPEN_FLAGS_NOINHERIT, (PEAOP2)NULL);
            if (arc == NO_ERROR) {
                // set the initial number of free bytes
                pipe.bytes = PIPE_SIZE_STDIN;
            }
        }
        break;
    case OutPipe:
    case ErrPipe:
        // create our end of the pipe
        sprintf(pathBuf, "\\pipe\\Qt5\\%08lX\\QProcess\\%p\\%s",
                ppib->pib_ulpid, this->q_func(),
                name ? name : type == OutPipe ? "Stdout" : "Stderr");
        arc = DosCreateNPipe(pathBuf, &pipe.server,
                             NP_ACCESS_INBOUND | NP_NOINHERIT,
                             NP_NOWAIT | NP_TYPE_BYTE | 1,
                             0, type == OutPipe ? PIPE_SIZE_STDOUT : PIPE_SIZE_STDERR, 0);
        if (arc == NO_ERROR) {
            DosConnectNPipe(pipe.server);
            // ensure the other end blocks (vital for process->process redirections)
            DosSetNPHState(pipe.server, NP_WAIT);
            // open the client end of the pipe
            ULONG action = 0;
            arc = DosOpen(pathBuf, &pipe.client, &action, 0, FILE_NORMAL, FILE_OPEN,
                          OPEN_ACCESS_WRITEONLY | OPEN_SHARE_DENYREADWRITE |
                          OPEN_FLAGS_NOINHERIT, (PEAOP2)NULL);
        }
        break;
    }

    if (arc != NO_ERROR) {
        qWarning("QProcessPrivate::createPipe: %s(%s) returned %lu",
                 pipe.server == INVALID_HFILE ? "DosCreateNPipe" : "DosOpen",
                 pathBuf, arc);
        setErrorAndEmit(QProcess::FailedToStart,
                        QProcess::tr("Resource error: %1").arg(QSystemError::os2String(arc)));
    }

    return arc == NO_ERROR;
}

void QProcessPrivate::destroyPipe(Channel::Pipe &pipe)
{
    pipe.bytes = 0;
    pipe.signaled = false;

    pipe.closePending = false;

    if (pipe.client != INVALID_HFILE) {
        closeHandle(pipe.client);
    }
    if (pipe.server != INVALID_HFILE) {
        // Note: We do not call DosDisConnectNPipe() as this is not necessary
        // and will only cause DosRead() on the other side to return
        // ERROR_PIPE_NOT_CONNECTED that will be interpreted by LIBC read() and
        // fread() as an error (ferror() will return true) and some programs
        // don't like it (e.g. 7z, rar32 when reading data from stdin).
        closeHandle(pipe.server);
    }
}

void QProcessPrivate::closeHandle(HFILE &handle)
{
    if (handle != INVALID_HFILE) {
        // Use LIBC close to let it free resources if this handle was imported
        // by spawn2. This should always succeed and call DosClose but fallback
        // to it just in case.
        if (close(handle) == -1)
            DosClose(handle);
    }
    handle = INVALID_HFILE;
}

void QProcessPrivate::closeChannel(Channel *channel)
{
    destroyPipe(channel->pipe);
}

/*
    Create the pipes to a QProcessPrivate::Channel.

    This function must be called in order: stdin, stdout, stderr
*/
bool QProcessPrivate::openChannel(Channel &channel)
{
    DEBUG(() << "type" << channel.type);

    PipeType type;

    if (&channel == &stdinChannel) {
        type = InPipe;
        if (inputChannelMode == QProcess::ForwardedInputChannel)
            return true;
    } else if (&channel == &stdoutChannel) {
        type = OutPipe;
        if (processChannelMode == QProcess::ForwardedChannels ||
            processChannelMode == QProcess::ForwardedOutputChannel)
            return true;
    } else if (&channel == &stderrChannel) {
        type = ErrPipe;
        if (processChannelMode == QProcess::MergedChannels ||
            processChannelMode == QProcess::ForwardedChannels ||
            processChannelMode == QProcess::ForwardedErrorChannel)
            return true;
    } else {
        Q_ASSERT(false);
        return false;
    }

    if (channel.type == Channel::Normal) {
        // we're piping this channel to our own process
        return createPipe(type, channel.pipe);
    } else if (channel.type == Channel::Redirect) {
        // we're redirecting the channel to/from a file
        QByteArray fname = QFile::encodeName(channel.file);
        DEBUG(() << "fname" << fname);

        APIRET rc;
        if (&channel == &stdinChannel) {
            // try to open in read-only mode
            ULONG action = 0;
            rc = DosOpen(fname, &channel.pipe.client, &action, 0, FILE_NORMAL, FILE_OPEN,
                         OPEN_ACCESS_READONLY | OPEN_SHARE_DENYNONE | OPEN_FLAGS_NOINHERIT, (PEAOP2)NULL);

            if (rc == NO_ERROR)
                return true; // success

            DEBUG(() << "DosOpen rc" << rc);
            setErrorAndEmit(QProcess::FailedToStart,
                            QProcess::tr("Could not open input redirection for reading"));
        } else {
            int mode = FILE_CREATE;
            if (channel.append)
                mode |= FILE_OPEN;
            else
                mode |= FILE_TRUNCATE;
            ULONG action = 0;
            rc = DosOpen(fname, &channel.pipe.client, &action, 0, FILE_NORMAL, mode,
                         OPEN_ACCESS_WRITEONLY | OPEN_SHARE_DENYWRITE |
                         OPEN_FLAGS_NOINHERIT, (PEAOP2)NULL);

            if (rc == NO_ERROR && channel.append) {
                ULONG actual = 0;
                rc = DosSetFilePtr(channel.pipe.client, 0, FILE_END, &actual);
            }
            if (rc == NO_ERROR)
                return true; // success

            DEBUG(() << "DosOpen rc" << rc);
            setErrorAndEmit(QProcess::FailedToStart,
                            QProcess::tr("Could not open output redirection for writing"));
        }

        return false;
    } else {
        Q_ASSERT_X(channel.process, "QProcess::start", "Internal error");

        // the first process started is the server, the second one is the client.
        // the server stores handles to both ends in its channel.pipe member;
        // the other part will pickup the handle of the client end from there.

        if (channel.type == Channel::PipeSource) {
            // we are the source
            Q_ASSERT(&channel == &stdoutChannel);
            Q_ASSERT(channel.process->stdinChannel.process == this &&
                     channel.process->stdinChannel.type == Channel::PipeSink);
            if (channel.process->stdinChannel.pipe.server != INVALID_HFILE) {
                // the other process has already started and became the server
            } else {
                // note: InPipe, since the type is relative to the other side
                createPipe(InPipe, channel.pipe, "Source");
            }
        } else {
            // we are the sink (and the server)
            Q_ASSERT(channel.type == Channel::PipeSink);
            Q_ASSERT(&channel == &stdinChannel);
            Q_ASSERT(channel.process->stdoutChannel.process == this &&
                     channel.process->stdoutChannel.type == Channel::PipeSource);
            if (channel.process->stdoutChannel.pipe.server != INVALID_HFILE) {
                // the other process has already started and became the server
            } else {
                // note: OutPipe, since the type is relative to the other side
                createPipe(OutPipe, channel.pipe, "Sink");
            }
        }

        return true;
    }
}

static int qt_startProcess(const QString &program, const QStringList &arguments,
                           const QString &workingDirectory,
                           const QStringList *environment, int stdfds[3],
                           bool threadSafe, bool detached = false)
{
    // Don't go further if we got an empty program name: a file with such a name
    // cannot exist on OS/2 (and many QFileInfo funcs are undefined for this
    // case0 - simply return ENOENT to the caller.
    if (program.isEmpty()) {
        errno = ENOENT;
        return -1;
    }

    // Encode the program name.
    QByteArray programName = QFile::encodeName(program);

    // Try to find the executable (_path2 will set ENOENT etc when appropriate).
    QByteArray fullProgramName(CCHMAXPATH * 2, 0);
    if (_path2(programName, ".exe;.cmd;.bat", /* in order of CMD.EXE's precedence */
               fullProgramName.data(), fullProgramName.size()) == -1)
        return -1;

    DEBUG(("found \"%s\" for \"%s\"",
           qPrintable(QFile::decodeName(fullProgramName)),
           qPrintable(QFile::decodeName(programName))));

    // Create an argument list with the right number of elements (one extra is for
    // the program name and one for the terminating 0).
    const char **argv = new const char *[arguments.count() + 2];

    // Set the final argument to 0
    argv[arguments.count() + 1] = 0;

    // Add the program name to the argument list (as given, by convention).
    argv[0] = programName.data();

    // Add every argument to the list
    for (int i = 0; i < arguments.count(); ++i) {
        QString arg = arguments.at(i);
        argv[i + 1] = qstrdup(arg.toLocal8Bit().constData());
    }

    // Duplicate the environment.
    int envc = 0;
    char **envv;
    if (environment && environment->count()) {
        bool seenPATH = false;
        bool seenCOMSPEC = false;

        envv = new char *[environment->count() + 1 + 2 /* may be PATH + COMSPEC */];
        for (; envc < environment->count(); ++envc) {
            QString item = environment->at(envc);
            envv[envc] = qstrdup(item.toLocal8Bit().constData());
            if (!seenPATH)
                seenPATH = !qstrncmp(envv[envc], "PATH=", 4);
            if (!seenCOMSPEC)
                seenCOMSPEC = !qstrncmp(envv[envc], "COMSPEC=", 8);
        }
        if (!seenPATH) {
            // inherit PATH if missing (for convenience)
            // (note that BEGINLIBPATH and ENDLIBPATH, if any, are automatically
            // inherited, while LIBPATH is always a global setting)
            QByteArray path = qgetenv("PATH");
            path.prepend("PATH=");
            envv[envc++] = qstrdup(path);
        }
        // inherit COMSPEC if missing (to let the child start .cmd and .bat)
        if (!seenCOMSPEC) {
            QByteArray comspec = qgetenv("COMSPEC");
            comspec.prepend("COMSPEC=");
            envv[envc++] = qstrdup(comspec);
        }
        envv[envc] = 0;
    } else {
        // inherit the parent environment
        envv = environ;
    }

    // Prepare the working directory if it's non-empty
    const char *workDirPtr = NULL;
    QByteArray workDir;
    if (!workingDirectory.isEmpty()) {
        workDir = QFile::encodeName(workingDirectory);
        workDirPtr = workDir;
    }

    DEBUG(() << "curdir" << QDir::currentPath() << "workingDirectory" << workingDirectory);

    // Add the program's dir to BEGINLIBPATH to make sure the DLLs are
    // serached there first (no, the OS/2 loader does't do it itself).
    QFileInfo fullProgramInfo(QFile::decodeName(fullProgramName));
    QString fullPath = QDir::toNativeSeparators(fullProgramInfo.absolutePath());

    APIRET arc;
    QStringList paths;
    char libPathBuf[4096]; // @todo isn't it too small?
    bool prependedLibPath = false;

    arc = DosQueryExtLIBPATH(libPathBuf, BEGIN_LIBPATH);
    Q_ASSERT(arc == NO_ERROR);
    if (arc == NO_ERROR) {
        QString path = QFile::decodeName(libPathBuf);
        paths = path.split(QLatin1Char(';'), Qt::SkipEmptyParts);
        if (paths.contains(fullPath, Qt::CaseInsensitive)) {
            DEBUG(("\"%s\" is already in BEGINLIBPATH",
                   qPrintable(fullPath)));
        } else {
            DEBUG(("prepending \"%s\" to BEGINLIBPATH",
                   qPrintable(fullPath)));
            prependedLibPath = true;
            QByteArray newLibPath = libPathBuf;
            newLibPath.prepend(';').prepend(QFile::encodeName(fullPath));
            DosSetExtLIBPATH(newLibPath, BEGIN_LIBPATH);
        }
    }

    int mode = detached ? P_SESSION : P_NOWAIT;

    // Get the application type of our program (note that we cannot use
    // DosGetInfoBlocks/PIB because it could be overwritten there by e.g.
    // morphing from VIO to PM which doesn't cancel the fact that we
    // are VIO from the OS/2 loader's POV.
    ULONG flags;
    arc = DosQueryAppType(QFile::encodeName(qAppFileName()), &flags);
    if (arc == NO_ERROR && (flags & 0x7) != FAPPTYP_WINDOWAPI) {
        // We are originally not the PM application and thus DosExecPgm()
        // won't be able to start PM applications directly (note that the
        // other way around it works although undocumented). Check what
        // the target program is.
        arc = DosQueryAppType(fullProgramName, &flags);
        if (arc == NO_ERROR && (flags & 0x7) == FAPPTYP_WINDOWAPI) {
            // it's PM, we have to use P_PM (almost the same as P_SESSION)
            mode = P_PM;
        }
    }

    // NOTE: After detecting the application type which may override mode!
    if (detached)
        mode |= P_UNRELATED;
    if (threadSafe)
        mode |= P_2_THREADSAFE;

#if defined(QPROCESS_DEBUG)
    DEBUG(("executable \"%s\"", qPrintable(QFile::decodeName(fullProgramName))));
    for (int i = 0; argv[i]; ++i) {
        DEBUG((" arg[%d] \"%s\"", i, qPrintable(QFile::decodeName(argv[i]))));
    }
    DEBUG(("mode 0x%x (threadSafe %d)", mode, threadSafe));

    // Redirect qDebug output to a file as spawn2 temporarily changes stdout/sterr.
    QtMessageHandler oldMsgHandler = qInstallMessageHandler(msgHandler);
#endif

    // Finally, start the thing
    int pid = spawn2(mode, fullProgramName, argv, workDirPtr, envv, stdfds);

#if defined(QPROCESS_DEBUG)
    qInstallMessageHandler(oldMsgHandler);
#endif

    DEBUG(("started pid %d errno %d", pid, errno));

    if (prependedLibPath) {
        // restore BEGINLIBPATH
        DosSetExtLIBPATH(libPathBuf, BEGIN_LIBPATH);
    }

    // Clean up duplicated memory.
    for (int i = 1 /* 0 is programName */; i <= arguments.count(); ++i)
        delete [] argv[i];
    delete [] argv;
    if (envv != environ) {
        for (int i = 0; i < envc; ++i)
            delete [] envv[i];
        delete [] envv;
    }

    return pid;
}

void QProcessPrivate::startProcess()
{
    Q_Q(QProcess);

    DEBUG(());

    // Initialize pipes
    if (!openChannel(stdinChannel) ||
        !openChannel(stdoutChannel) ||
        !openChannel(stderrChannel)) {
        // setErrorAndEmit() is called inside openChannel()
        cleanup();
        return;
    }

    stdinChannel.pipe.signaled = false;
    stdoutChannel.pipe.signaled = false;
    stderrChannel.pipe.signaled = false;

    procKey = QProcessManager::addProcess(q);

    if (procKey == QProcessManager::InvalidProcKey) {
        // setErrorAndEmit() is called inside addProcess()
        cleanup();
        return;
    }

    // Start the process (platform dependent)

    q->setProcessState(QProcess::Starting);

    int rc = 0;

    int stdfds[3] = {0};

    if (inputChannelMode != QProcess::ForwardedInputChannel) {
        HFILE handle = stdinChannel.pipe.client;
        if (stdinChannel.type == Channel::PipeSink) {
            // process -> process redirection
            if (stdinChannel.pipe.server != INVALID_HFILE) {
                // we are the server
                handle = stdinChannel.pipe.server;
            } else {
                // we are the client, use the server's variable
                handle = stdinChannel.process->stdoutChannel.pipe.client;
            }
        }
        Q_ASSERT(handle != INVALID_HFILE);
        stdfds[0] = handle;
    }
    if (processChannelMode != QProcess::ForwardedChannels) {
        if (processChannelMode != QProcess::ForwardedOutputChannel) {
            HFILE handle = stdoutChannel.pipe.client;
            if (stdoutChannel.type == Channel::PipeSource) {
                // process -> process redirection
                if (stdoutChannel.pipe.server != INVALID_HFILE) {
                    // we are the server
                    handle = stdoutChannel.pipe.server;
                } else {
                    // we are the client, use the server's variable
                    handle = stdoutChannel.process->stdinChannel.pipe.client;
                }
            }
            Q_ASSERT(handle != INVALID_HFILE);
            stdfds[1] = handle;
        }
        if (processChannelMode != QProcess::ForwardedErrorChannel) {
            if (processChannelMode == QProcess::MergedChannels) {
                stdfds[2] = 1;
            } else {
                Q_ASSERT(stderrChannel.pipe.client != INVALID_HFILE);
                stdfds[2] = stderrChannel.pipe.client;
            }
        }
    }

    int pid = -1;
    if (rc != -1) {
        QStringList env = environment.toStringList();
        pid = qt_startProcess(program, arguments, workingDirectory, &env, stdfds, threadSafe);
    }

    if (rc == -1 || pid == -1) {
        // Cleanup, report error and return
        q->setProcessState(QProcess::NotRunning);
        if (rc == -1) {
            // handle duplication failed
            setErrorAndEmit(QProcess::FailedToStart,
                            QProcess::tr("Resource error: %1").arg(qt_error_string(errno)));
        } else {
            DEBUG(("spawn2 failed: %s", qPrintable(qt_error_string(errno))));
            setErrorAndEmit(QProcess::FailedToStart, qt_error_string(errno));
        }
        QProcessManager::removeProcess(procKey);
        procKey = QProcessManager::InvalidProcKey;
        cleanup();
        return;
    }

    this->pid = Q_PID(pid);

    DEBUG(() << "pid" << pid);

    // close the client ends inherited by the started process (it's necessary to
    // make sure that the started process owns the only handle to the client end
    // and when it closes this handle the other party will notice it and e.g.
    // stop waiting for new data).

    if (stdinChannel.type == Channel::PipeSink) {
        // process -> process redirection
        if (stdinChannel.pipe.server != INVALID_HFILE) {
            // we are the server, leave the handle for the other party
        } else {
            // we are the client, close the handle
            closeHandle(stdinChannel.process->stdoutChannel.pipe.client);
        }
    } else {
        if (stdinChannel.pipe.client != INVALID_HFILE) {
            closeHandle(stdinChannel.pipe.client);
        }
    }
    if (stdoutChannel.type == Channel::PipeSource) {
        // process -> process redirection
        if (stdoutChannel.pipe.server != INVALID_HFILE) {
            // we are the server, leave the handle for the other party
        } else {
            // we are the client, close the handle
            closeHandle(stdoutChannel.process->stdinChannel.pipe.client);
        }
    } else {
        if (stdoutChannel.pipe.client != INVALID_HFILE) {
            closeHandle(stdoutChannel.pipe.client);
        }
    }
    if (stderrChannel.pipe.client != INVALID_HFILE) {
        closeHandle(stderrChannel.pipe.client);
    }

    // give the process a chance to start ...
    DosSleep(100);

    _q_startupNotification();
}

bool QProcessPrivate::processStarted(QString * /*errorMessage*/)
{
    // we don't actually wait for any notification from the child process
    // assuming it has been started as long as spawnvpe() returns success
    return processState == QProcess::Starting;
}

// static
qint64 QProcessPrivate::bytesAvailableFromPipe(HFILE hpipe, bool *closed)
{
    qint64 bytes = 0;
    if (hpipe != INVALID_HFILE) {
        ULONG state, dummy;
        AVAILDATA avail;
        APIRET arc = DosPeekNPipe(hpipe, 0, 0, &dummy, &avail, &state);
        Q_ASSERT(arc == NO_ERROR);
        Q_UNUSED(arc);
        bytes = (qint64)avail.cbpipe;
        if (closed) {
            *closed = state != NP_STATE_CONNECTED;
        }
    }
    return bytes;
}

qint64 QProcessPrivate::bytesAvailableInChannel(const Channel *channel) const
{
    Q_ASSERT(channel->pipe.server != INVALID_HFILE);
    qint64 bytes = 0;
    if (!dying) {
        Q_ASSERT(channel->pipe.signaled);
        // reuse the number we got in _q_notified()
        bytes = channel->pipe.bytes;
    } else {
        if (stdoutChannel.type == QProcessPrivate::Channel::Normal)
            bytes = bytesAvailableFromPipe(channel->pipe.server);
    }

    DEBUG(("%s = %lld", channel == &stdoutChannel ? "stdout" : "stderr", bytes));
    return bytes;
}

qint64 QProcessPrivate::readFromChannel(const Channel *channel, char *data, qint64 maxlen)
{
    ULONG actual = 0;
    APIRET arc = DosRead(channel->pipe.server, data, maxlen, &actual);

    qint64 bytesRead = -1;
    if (arc == NO_ERROR) {
        bytesRead = (qint64)actual;
    }

    DEBUG(("%s, %p \"%s\", %lld = %lld", channel == &stdoutChannel ? "stdout" : "stderr",
           data, qt_prettyDebug(data, bytesRead, 16).constData(), maxlen, bytesRead));
    return bytesRead;
}

void QProcessPrivate::tryCloseStdinPipe()
{
    Q_ASSERT(stdinChannel.pipe.closePending);

    if (stdinChannel.pipe.closePending) {
        // check if the other end has read everything from the pipe buf so that
        // it's safe to close it (satisfy the closeWriteChannel() request) now
        if (stdinChannel.pipe.bytes == PIPE_SIZE_STDIN) {
            closeChannel(&stdinChannel);
        }
    }
}

bool QProcessPrivate::writeToStdin()
{
    const char *data = writeBuffer.readPointer();
    const qint64 bytesToWrite = writeBuffer.nextDataBlockSize();

    QMutexLocker lock(QProcessManager::pipeStateLock());

    // Reset the number of bytes before writing to the pipe. This makes sure
    // this method will be called again only after QProcessManager::run() sets
    // bytes to non-zero and sends us a signal. Note that we do it under the
    // lock to avoid cases when ProcessManager::run() overwrites this bytes
    // field with an outdated value from the previous pipe event (that could get
    // reported again due to another event fired on behalf of some other pipe).
    stdinChannel.pipe.bytes = 0;

    ULONG actual = 0;
    APIRET arc = DosWrite(stdinChannel.pipe.server, data, bytesToWrite, &actual);

    DEBUG(("DosWrite(%p \"%s\", %lld, %ld) == %lu", data,
           qt_prettyDebug(data, bytesToWrite, 16).constData(), bytesToWrite, actual, arc));
    if (arc != NO_ERROR) {
        closeChannel(&stdinChannel);
        setErrorAndEmit(QProcess::WriteError);
        return false;
    }

    lock.unlock();

    writeBuffer.free(actual);
    if (!emittedBytesWritten && actual != 0) {
        emittedBytesWritten = true;
        emit q_func()->bytesWritten(actual);
        emittedBytesWritten = false;
    }
    return true;
}

void QProcessPrivate::terminateProcess()
{
    DEBUG(() << "pid" << pid);
    if (pid) {
        // First, try to Close the main window if there is one. This is the
        // safest approach as it will give the app an opportunity to save its
        // data via a normal route (and also to refuse to close).
        HSWITCH hswitch = WinQuerySwitchHandle(NULLHANDLE, pid);
        DEBUG(() << "hswitch" << hex << hswitch);
        if (hswitch != NULLHANDLE) {
            SWCNTRL swcntrl;
            memset(&swcntrl, 0, sizeof(swcntrl));
            APIRET rc = WinQuerySwitchEntry(hswitch,  &swcntrl);
            // WinQuerySwitchEntry will return a switch entry of the parent
            // process if the specfied one doesn't have a separate session
            // (e.g. it's a related console app); ignore this case.
            if (rc == NO_ERROR && swcntrl.idProcess == pid)
            {
                // first, ensure that the Close action is enabled in the main frame
                // window (otherwise WM_SYSCOMMAND/SC_CLOSE will be ignored)
                HWND hwndSysMenu = WinWindowFromID(swcntrl.hwnd, FID_SYSMENU);
                if (hwndSysMenu) {
                    WinPostMsg(hwndSysMenu, MM_SETITEMATTR,
                               MPFROM2SHORT(SC_CLOSE, TRUE),
                               MPFROM2SHORT(MIA_DISABLED, 0));
                }
                WinPostMsg(swcntrl.hwnd, WM_SYSCOMMAND,
                           MPFROM2SHORT(SC_CLOSE, CMDSRC_OTHER),
                           MPFROMLONG(FALSE));
                DEBUG(("Posted SC_CLOSE to main window's system menu"));
                return;
            }
        }

        // Then, try to locate the message queue and post a quit message there.
        // Not as safe as the above (not all apps will handle WM_QUIT to save
        // their data) but better than killing. Note that we don't know which
        // thread runs the main message loop so post to each one we find.
        for (ULONG tid = 1; tid <= 255; ++tid) {
            if (DosVerifyPidTid(pid, tid) == NO_ERROR) {
                HMQ hmq = WinQueueFromID(NULLHANDLE, pid, tid);
                if (hmq != NULLHANDLE) {
                    WinPostQueueMsg(hmq, WM_QUIT, NULL, NULL);
                    DEBUG(("Posted WM_QUIT to message queue of thread %ld", tid));
                }
            }
        }
    }
}

void QProcessPrivate::killProcess()
{
    DEBUG(() << "pid" << pid);
    if (pid)
        DosKillProcess(DKP_PROCESS, pid);
}

/*
   Returns the difference between msecs and elapsed. If msecs is -1,
   however, -1 is returned.
*/
static int qt_timeout_value(int msecs, int elapsed)
{
    if (msecs == -1)
        return -1;

    int timeout = msecs - elapsed;
    return timeout < 0 ? 0 : timeout;
}

bool QProcessPrivate::waitForStarted(int /*msecs*/)
{
    if (processState == QProcess::Running)
        return true;

    if (processError == QProcess::FailedToStart)
        return false;

    setError(QProcess::Timedout);
    return false;
}

bool QProcessPrivate::waitFor(WaitCond cond, int msecs)
{
    Q_Q(QProcess);

#if defined QPROCESS_DEBUG
    const char *condStr = cond == WaitReadyRead ? "ReadyRead" :
                          cond == WaitBytesWritten ? "BytesWritten" :
                          cond == WaitFinished ? "Finished" : "???";
    DEBUG(("pid %d, %s, %d", pid, condStr, msecs));
#endif

    QElapsedTimer stopWatch;
    stopWatch.start();

    APIRET arc;
    bool ret = false;

    ensureWaitSem();
    waitMode = WaitMode;

    int notifyResult = 0;

    // QProcessManager::run() could post a method invocation before noticing we
    // entered waitMode, process it now to avoid an endless hang in wait state
    // due to the absense of the notification via the semaphore
    QCoreApplication::sendPostedEvents(q, QEvent::MetaCall);
    if (!QCoreApplication::instance()) {
        // however, if there is no Q*Application, _q_notified() won't be called
        // by the above, only removed from the queue. So we need a manual call.
        notifyResult = _q_notified(CanAll);
    } else {
        // When waitMode is non-zero, _q_notify accumulates its return values in it for us.
        notifyResult = waitMode;
    }

    forever {
        bool done = false;

        if (notifyResult & CanDie) {
            done = true;
            ret = cond == WaitFinished;
            break;
        }

        if (cond == WaitReadyRead) {
            if ((currentReadChannel == QProcess::StandardOutput && (notifyResult & CanReadStdOut)) ||
                (currentReadChannel == QProcess::StandardError && (notifyResult & CanReadStdErr))) {
                done = ret = true;
                break;
            }
        } else if (cond == WaitBytesWritten) {
            if (notifyResult & CanWrite) {
                done = ret = true;
                break;
            }
            if (writeBuffer.isEmpty()) {
                done = true;
                break;
            }
        }

        // reset all signaled flags
        stdoutChannel.pipe.signaled = false;
        stderrChannel.pipe.signaled = false;
        stdinChannel.pipe.signaled = false;

        if (done)
            break;

        // wait for the new signals
        int timeout = qt_timeout_value(msecs, stopWatch.elapsed());
        DEBUG(() << "timeout" << timeout);
        qDosNI(arc = DosWaitEventSem(waitSem, (ULONG)timeout));

        if (arc == ERROR_TIMEOUT) {
            setError(QProcess::Timedout);
            break;
        } else if (arc != NO_ERROR) {
            Q_ASSERT(arc == NO_ERROR);
            break;
        }

        // check all conditions upon the signal from QProcessManager::run()
        notifyResult = _q_notified(CanAll);
    }

    waitMode = 0;

    ULONG postCnt = 0;
    arc = DosResetEventSem(waitSem, &postCnt);
    if (arc == NO_ERROR && postCnt) {
        // QProcessManager::run() posted the semaphore before seeing that we
        // left waitMode, repost it as a method invocation to avoid signal loss
        QMetaObject::invokeMethod(q, "_q_notified", Qt::QueuedConnection,
                                  Q_ARG(int, QProcessPrivate::CanAll));
    }

    DEBUG(("%s, %d returns %d", condStr, msecs, ret));
    return ret;
}

bool QProcessPrivate::waitForReadyRead(int msecs)
{
    return waitFor(WaitReadyRead, msecs);
}

bool QProcessPrivate::waitForBytesWritten(int msecs)
{
    return waitFor(WaitBytesWritten, msecs);
}

bool QProcessPrivate::waitForFinished(int msecs)
{
    return waitFor(WaitFinished, msecs);
}

void QProcessPrivate::findExitCode()
{
    // note: this method is unconditionally called from QProcess destructor
    // to make sure the process manager removes the watcher even in such a rare
    // case when the process is still running after killing it and waiting
    // for termination (in which case the child termination code path that
    // normally calls findExitCode() won't be walked)

    if (procKey != QProcessManager::InvalidProcKey) {
        QProcessManager::removeProcess(procKey);
        procKey = QProcessManager::InvalidProcKey;
    }
}

bool QProcessPrivate::waitForDeadChild()
{
    DEBUG(() << "pid" << pid);
    Q_ASSERT(pid != 0);

    // check if our process is dead
    int exitStatus;
    pid_t waitResult = waitpid(pid, &exitStatus, WNOHANG);
    if (waitResult == (pid_t)pid) {
        crashed = !WIFEXITED(exitStatus);
        exitCode = WEXITSTATUS(exitStatus);
        DEBUG(() << "dead with exitCode" << exitCode << ", crashed?" << crashed);
        return true;
    }

    DEBUG(() << "not dead!" << waitResult << "errno" << errno << strerror(errno));
    return false;
}

int QProcessPrivate::_q_notified(int flags)
{
    DEBUG(() << "flags" << hex << flags << "waitMode" << hex << waitMode << dec << "pid" << pid);

    int ret = 0;

    // note: in all read cases below, we look for the number of bytes actually
    // available to sort out (ignore) outdated notifications from
    // QProcessManager::run() indicated by 0 bytes and to detect closures of
    // the remote end of the pipe indicated by bytes < 0

    // note 2: the Channel::Pipe::signaled flag may be set when we enter this
    // method which means that waitFor() found more than one posted
    // _q_notified() call of the same type in the event queue; we need to
    // gracefully handle them all (they may inform about separate payloads)

    if ((flags & CanReadStdOut) && stdoutChannel.type != Channel::PipeSource) {
        Q_ASSERT(!stdoutChannel.pipe.signaled || waitMode);
        bool closed = false;
        qint64 bytes = bytesAvailableFromPipe(stdoutChannel.pipe.server, &closed);
        if (bytes || closed) {
            stdoutChannel.pipe.bytes = bytes;
            stdoutChannel.pipe.signaled = true;
            if (_q_canReadStandardOutput())
                ret |= CanReadStdOut;
            if (closed && bytes) {
                // ask _q_canReadStandardOutput() to close the pipe by setting
                // bytes to 0 (only if not already done by the previous call)
                stdoutChannel.pipe.bytes = 0;
                _q_canReadStandardOutput();
            }
            if (!waitMode) {
                // reset the signaled flag
                stdoutChannel.pipe.signaled = false;
            }
#if defined QPROCESS_DEBUG
        } else if (!waitMode) {
            DEBUG(("stale CanReadStdOut signal!"));
#endif
        }
    }

    if ((flags & CanReadStdErr) && processChannelMode != QProcess::MergedChannels) {
        Q_ASSERT(!stderrChannel.pipe.signaled || waitMode);
        bool closed = false;
        qint64 bytes = bytesAvailableFromPipe(stderrChannel.pipe.server, &closed);
        if (bytes || closed) {
            stderrChannel.pipe.bytes = bytes;
            stderrChannel.pipe.signaled = true;
            if (_q_canReadStandardError())
                ret |= CanReadStdErr;
            if (closed && bytes) {
                // ask _q_canReadStandardError() to close the pipe by setting
                // bytes to 0 (only if not already done by the previous call)
                stderrChannel.pipe.bytes = 0;
                _q_canReadStandardError();
            }
            if (!waitMode) {
                // reset the signaled flag
                stderrChannel.pipe.signaled = false;
            }
#if defined QPROCESS_DEBUG
        } else if (!waitMode) {
            DEBUG(("stale CanReadStdErr signal!"));
#endif
        }
    }

    if ((flags & CanWrite) && stdinChannel.type != Channel::PipeSink) {
        if (stdinChannel.pipe.bytes) {
            stdinChannel.pipe.signaled = true;
            if (_q_canWrite())
                ret |= CanWrite;
            if (!waitMode) {
                // reset the signaled flag
                stdinChannel.pipe.signaled = false;
            }
        }
    }

    if (flags & CanDie) {
        // Note: don't deliver _q_processDied if the process is already dead.
        if (dying || processState == QProcess::NotRunning) {
            ret |= CanDie;
        } else {
            if (_q_processDied())
                ret |= CanDie;
        }
    }

    if (waitMode)
        waitMode |= ret;

    DEBUG(() << "ret" << hex << ret << "waitMode" << hex << waitMode);
    return ret;
}

bool QProcessPrivate::startDetached(qint64 *pid)
{
    if ((stdinChannel.type == Channel::Redirect && !openChannel(stdinChannel))
            || (stdoutChannel.type == Channel::Redirect && !openChannel(stdoutChannel))
            || (stderrChannel.type == Channel::Redirect && !openChannel(stderrChannel))) {
        closeChannel(&stdinChannel);
        closeChannel(&stdoutChannel);
        closeChannel(&stderrChannel);
        return false;
    }

    int stdfds[3] = {0};

    if (stdinChannel.type == Channel::Redirect) {
        HFILE handle = stdinChannel.pipe.client;
        Q_ASSERT(handle != INVALID_HFILE);
        stdfds[0] = handle;
    }
    if (stdoutChannel.type == Channel::Redirect) {
        HFILE handle = stdoutChannel.pipe.client;
        Q_ASSERT(handle != INVALID_HFILE);
        stdfds[1] = handle;
    }
    if (stderrChannel.type == Channel::Redirect) {
        HFILE handle = stderrChannel.pipe.client;
        Q_ASSERT(handle != INVALID_HFILE);
        stdfds[2] = handle;
    }

    QStringList env = environment.toStringList();
    int startedPid = qt_startProcess(program, arguments, workingDirectory,
                                     &env, stdfds, threadSafe, true /* detached */);

    DEBUG(() << "startedPid" << startedPid);

    if (stdinChannel.pipe.client != INVALID_HFILE) {
        closeHandle(stdinChannel.pipe.client);
    }
    if (stdoutChannel.pipe.client != INVALID_HFILE) {
        closeHandle(stdoutChannel.pipe.client);
    }
    if (stderrChannel.pipe.client != INVALID_HFILE) {
        closeHandle(stderrChannel.pipe.client);
    }

    if (startedPid == -1)
        return false;

    if (pid)
        *pid = startedPid;
    return true;
}

#endif // QT_CONFIG(process)

QT_END_NAMESPACE
