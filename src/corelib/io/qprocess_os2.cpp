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

//#define QPROCESS_DEBUG

#if defined(__INNOTEK_LIBC__)
#include <emx/umalloc.h> // for _lmalloc()
#endif

#ifndef QT_NO_PROCESS

#if defined QPROCESS_DEBUG
#include "qdebug.h"
#include "qstring.h"
#include <ctype.h>

#include <qthread.h>

QT_BEGIN_NAMESPACE

// defined in qprocess.cpp
extern QByteArray qt_prettyDebug(const char *data, int len, int maxSize);

// redirect qDebug output to a file in the root directory of the current drive
static void msgHandler(QtMsgType, const char *msg)
{
    static FILE *f = 0;
    if (!f) {
        f = fopen("\\qprocess.dbg", "wb");
        setbuf(f, NULL);
    }
    fprintf(f, "%s\n", msg);
}

static int setMsgHandler() { qInstallMsgHandler(msgHandler); return 0; }
Q_CONSTRUCTOR_FUNCTION(setMsgHandler)

QT_END_NAMESPACE

#define DEBUG(a) qDebug a
#else
#define DEBUG(a) do {} while(0)
#endif

#include "qplatformdefs.h"

#include "qprocess.h"
#include "qprocess_p.h"

#include <QtCore/qt_os2.h>

#include <private/qcoreapplication_p.h>
#include <private/qthread_p.h>
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

extern QString qAppFileName();

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
    DEBUG(("QProcessManager::addProcess(%p)", process));

    QMutexLocker locker(&mutex);
    Q_ASSERT(instance);

    // lazily enable SIGCHLD handler and start the worker
    if (instance->eventSemGuard.testAndSetAcquire(0, 1)) {
        instance->installSigHandler();
        instance->start();
    }

    USHORT procKey = instance->lastProcKey + 1;
    if (procKey > MaxProcKey) {
        // limit reached, find an unused number
        procKey = 0;
        while (++procKey <= MaxProcKey &&
               instance->processes.contains(procKey));
        Q_ASSERT(procKey <= MaxProcKey);
        if (procKey > MaxProcKey) {
            // oops, no more free keys!
            process->setErrorString(QLatin1String("Internal error: Too many processes"));
            return InvalidProcKey;
        }
    } else {
        instance->lastProcKey = procKey;
    }

    // attach the semahpore to the pipes of the process
    APIRET arc = NO_ERROR;
    QProcessPrivate *d = process->d_func();
    if (d->stdinChannel.type == QProcessPrivate::Channel::Normal &&
        d->stdinChannel.pipe.server != HPIPE(~0)) {
        arc = DosSetNPipeSem(d->stdinChannel.pipe.server, (HSEM)instance->eventSem,
                             toPipeKey(procKey, QProcessPrivate::InPipe));
    }
    if (arc == NO_ERROR &&
        d->stdoutChannel.type == QProcessPrivate::Channel::Normal &&
        d->stdoutChannel.pipe.server != HPIPE(~0)) {
        arc = DosSetNPipeSem(d->stdoutChannel.pipe.server, (HSEM)instance->eventSem,
                             toPipeKey(procKey, QProcessPrivate::OutPipe));
    }
    if (arc == NO_ERROR &&
        d->stderrChannel.type == QProcessPrivate::Channel::Normal &&
        d->stderrChannel.pipe.server != HPIPE(~0)) {
        arc = DosSetNPipeSem(d->stderrChannel.pipe.server, (HSEM)instance->eventSem,
                             toPipeKey(procKey, QProcessPrivate::ErrPipe));
    }
    if (arc != NO_ERROR) {
        process->setErrorString(QProcess::tr("Internal error: DOS error %1")
                                .arg(arc));
        if (procKey == instance->lastProcKey)
            --instance->lastProcKey;
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

    DEBUG(("QProcessManager::removeProcess(%p)", process));

    // to guarantee that the given procKey may be reused, we must close all
    // pipes in order to ensure that we won't get late NPSS_CLOSE for the
    // removed process with the key that may be already associated with a new one
    QProcessPrivate *d = process->d_func();
    d->destroyPipe(d->stdinChannel.pipe);
    d->destroyPipe(d->stdoutChannel.pipe);
    d->destroyPipe(d->stderrChannel.pipe);

    instance->processes.remove(procKey);

    // small optimization: released the highest key
    if (procKey == instance->lastProcKey)
        --instance->lastProcKey;
}

QProcessManager::QProcessManager()
    : refcnt(0), finish(false), eventSem(NULLHANDLE), sa_old_sigchld_handler(0)
{
    DEBUG(() << "QProcessManager::QProcessManager()");

    APIRET rc = DosCreateEventSem(NULL, &eventSem,
                                  DC_SEM_SHARED | DCE_AUTORESET | DCE_POSTONE,
                                  FALSE);
    Q_ASSERT(rc == NO_ERROR);
    Q_UNUSED(rc);

    lastProcKey = InvalidProcKey;
}

QProcessManager::~QProcessManager()
{
    DEBUG(() << "QProcessManager::~QProcessManager()");

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
    DEBUG(() << "QProcessManager::run() BEGIN");

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
            DEBUG(() << "QProcessManager::run(): child death signaled");
            foreach (QProcess *proc, processes) {
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

        DEBUG(() << "QProcessManager::run(): checking pipes");

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
            DEBUG(("  process %p", proc));

            int flags = 0;
            switch(type) {
            case QProcessPrivate::InPipe:
                Q_ASSERT(status == NPSS_CLOSE || status == NPSS_WSPACE);
                flags |= QProcessPrivate::CanWrite;
                // save the current number of free bytes in the pipe to let
                // _q_canWrite() go (this is also used in tryCloseStdinPipe())
                proc->d_func()->pipeData[type].bytes = pipeStates[i].usAvail;
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

    DEBUG(() << "QProcessManager::run() END");
}


void QProcessPrivate::init()
{
    waitMode = false;
    waitSem = NULLHANDLE;
    memset(pipeData, 0, sizeof(pipeData));

    procKey = QProcessManager::InvalidProcKey;

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
    APIRET rc = NO_ERROR;
    char pathBuf[CCHMAXPATH];

    pipe.server = HPIPE(~0);
    pipe.client = HFILE(~0);
    pipe.closePending = false;

    // we need the process identifier to guarantee pipe name unicity
    PPIB ppib = NULL;
    DosGetInfoBlocks(NULL, &ppib);

    switch (type) {
    case InPipe:
        // create our end of the pipe
        sprintf(pathBuf, "\\pipe\\Qt4\\%08lX\\QProcess\\%p\\%s",
                ppib->pib_ulpid, this->q_func(), name ? name : "Stdin");
        rc = DosCreateNPipe(pathBuf, &pipe.server,
                            NP_ACCESS_OUTBOUND | NP_NOINHERIT,
                            NP_NOWAIT | NP_TYPE_BYTE | 1,
                            PIPE_SIZE_STDIN, 0, 0);
        if (rc == NO_ERROR) {
            DosConnectNPipe(pipe.server);
            // ensure the other end blocks (vital for process->process redirections)
            DosSetNPHState(pipe.server, NP_WAIT);
            // open the client end of the pipe
            ULONG action = 0;
            rc = DosOpen(pathBuf, &pipe.client, &action, 0, FILE_NORMAL, FILE_OPEN,
                         OPEN_ACCESS_READONLY | OPEN_SHARE_DENYREADWRITE |
                         OPEN_FLAGS_NOINHERIT, (PEAOP2)NULL);
            if (rc == NO_ERROR) {
                // set the initial number of free bytes
                pipeData[type].bytes = PIPE_SIZE_STDIN;
            }
        }
        break;
    case OutPipe:
    case ErrPipe:
        // create our end of the pipe
        sprintf(pathBuf, "\\pipe\\Qt4\\%08lX\\QProcess\\%p\\%s",
                ppib->pib_ulpid, this->q_func(),
                name ? name : type == OutPipe ? "Stdout" : "Stderr");
        rc = DosCreateNPipe(pathBuf, &pipe.server,
                            NP_ACCESS_INBOUND | NP_NOINHERIT,
                            NP_NOWAIT | NP_TYPE_BYTE | 1,
                            0, type == OutPipe ? PIPE_SIZE_STDOUT : PIPE_SIZE_STDERR, 0);
        if (rc == NO_ERROR) {
            DosConnectNPipe(pipe.server);
            // ensure the other end blocks (vital for process->process redirections)
            DosSetNPHState(pipe.server, NP_WAIT);
            // open the client end of the pipe
            ULONG action = 0;
            rc = DosOpen(pathBuf, &pipe.client, &action, 0, FILE_NORMAL, FILE_OPEN,
                         OPEN_ACCESS_WRITEONLY | OPEN_SHARE_DENYREADWRITE |
                         OPEN_FLAGS_NOINHERIT, (PEAOP2)NULL);
        }
        break;
    }

    if (rc != NO_ERROR) {
        qWarning("QProcessPrivate::createPipe: %s(%s) returned %lu",
                 pipe.server == HPIPE(~0) ? "DosCreateNPipe" : "DosOpen",
                 pathBuf, rc);
    }

    return rc == NO_ERROR;
}

void QProcessPrivate::destroyPipe(Channel::Pipe &pipe)
{
    pipe.closePending = false;

    if (pipe.client != ~HFILE(~0)) {
        DosClose(pipe.client);
        pipe.client = HFILE(~0);
    }
    if (pipe.server != HPIPE(~0)) {
        // Note: We do not call DosDisConnectNPipe() as this is not necessary
        // and will only cause DosRead() on the other side to return
        // ERROR_PIPE_NOT_CONNECTED that will be interpreted by LIBC read() and
        // fread() as an error (ferror() will return true) and some programs
        // don't like it (e.g. 7z, rar32 when reading data from stdin).
        DosClose(pipe.server);
        pipe.server = HPIPE(~0);
    }
}

/*
    Create the pipes to a QProcessPrivate::Channel.

    This function must be called in order: stdin, stdout, stderr
*/
bool QProcessPrivate::createChannel(Channel &channel)
{
    Q_Q(QProcess);

    channel.pipe.server = HPIPE(~0);
    channel.pipe.client = HFILE(~0);
    channel.pipe.closePending = false;

    if (&channel == &stderrChannel && processChannelMode == QProcess::MergedChannels) {
        return true;
    }

    if (channel.type == Channel::Normal) {
        // we're piping this channel to our own process
        PipeType type = InPipe;
        if (&channel == &stdinChannel)
            type = InPipe;
        else if (&channel == &stdoutChannel)
            type = OutPipe;
        else if (&channel == &stderrChannel)
            type = ErrPipe;
        else
            Q_ASSERT(false);
        return createPipe(type, channel.pipe);
    } else if (channel.type == Channel::Redirect) {
        // we're redirecting the channel to/from a file
        QByteArray fname = QFile::encodeName(channel.file);

        APIRET rc;
        if (&channel == &stdinChannel) {
            // try to open in read-only mode
            ULONG action = 0;
            rc = DosOpen(fname, &channel.pipe.client, &action, 0, FILE_NORMAL, FILE_OPEN,
                         OPEN_ACCESS_READONLY | OPEN_FLAGS_NOINHERIT, (PEAOP2)NULL);

            if (rc == NO_ERROR)
                return true; // success

            q->setErrorString(QProcess::tr("Could not open input redirection for reading"));
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

            q->setErrorString(QProcess::tr("Could not open output redirection for writing"));
        }

        // could not open file
        processError = QProcess::FailedToStart;
        emit q->error(processError);
        cleanup();
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
            if (channel.process->stdinChannel.pipe.server != HPIPE(~0)) {
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
            if (channel.process->stdoutChannel.pipe.server != HPIPE(~0)) {
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
                           const QStringList *environment, bool detached = false)
{
    int mode = detached ? P_SESSION : P_NOWAIT;

    // Create argument list with right number of elements (one extra is for
    // the program name and one for the terminating 0). We also reserve 3
    // elements in front of the array for a posssible command processor call
    // insertion.
    const char **argvBase = new const char *[arguments.count() + 2 + 3];

    // Don't use leading elements by default
    const char **argv = argvBase + 3;
    // Set the final argument to 0
    argv[arguments.count() + 1] = 0;

    // Encode the program name.
    QByteArray programName = QFile::encodeName(program);

    // Add the program name to the argument list.
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

    // Set the working directory if it's non-empty
    QString curDir;
    if (!workingDirectory.isEmpty()) {
        curDir = QDir::currentPath();
        QDir::setCurrent(workingDirectory);
    }

    DEBUG(("qt_startProcess: workdir is \"%s\"", qPrintable(QDir::currentPath())));

    // try to find the executable
    QByteArray fullProgramName;

    // don't go further if we got an empty program name: a file with such a name
    // cannot exist on OS/2 and many QFileInfo funcs are undefined for this case
    if (!programName.isEmpty()) {
        APIRET arc;
        char pathBuf[CCHMAXPATH];
        QFileInfo programInfo(program);
        QStringList knownExts;
        knownExts << QLatin1String("exe") << QLatin1String("cmd")
                  << QLatin1String("bat"); // in order of CMD.EXE's precedence
        QByteArray path =
            QFile::encodeName(QDir::toNativeSeparators(programInfo.path()));
        if (path == ".")
            path.clear();
        // run through all known exe extensions (+ no extension case)
        for (int i = 0; i <= knownExts.size(); ++i) {
            QByteArray name;
            if (i == 0) {
                // no extension case, only if a known extension is already there
                if (knownExts.contains(programInfo.suffix(), Qt::CaseInsensitive))
                    name = QFile::encodeName(programInfo.fileName());
                else
                    continue;
            } else {
                name = QFile::encodeName(programInfo.fileName() +
                                         QLatin1String(".") + knownExts[i-1]);
            }
            if (!path.isEmpty()) {
                arc = DosSearchPath(0, path, name, pathBuf, sizeof(pathBuf));
            } else {
                arc = DosSearchPath(SEARCH_IGNORENETERRS | SEARCH_ENVIRONMENT |
                                    SEARCH_CUR_DIRECTORY, "PATH",
                                    name, pathBuf, sizeof(pathBuf));
            }

            if (arc == NO_ERROR) {
                fullProgramName = pathBuf;
                break;
            }
            if (arc != ERROR_FILE_NOT_FOUND && arc != ERROR_PATH_NOT_FOUND) {
                qWarning("qt_startProcess: DosSearchPath(%s) returned %lu",
                         qPrintable(QFile::decodeName(name)), arc);
                break;
            }
        }
    }

    DEBUG(("qt_startProcess: found \"%s\" for \"%s\"",
           qPrintable(QFile::decodeName(fullProgramName)),
           qPrintable(QFile::decodeName(programName))));

    int pid = -1;
    if (fullProgramName.isEmpty()) {
        // return "No such file or directory"
        errno = ENOENT;
    } else {
        // add the program's dir to BEGINLIBPATH to make sure the DLLs are
        // serached there first (no, the OS/2 loader does't do it itself)
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
            paths = path.split(QLatin1Char(';'), QString::SkipEmptyParts);
            if (paths.contains(fullPath, Qt::CaseInsensitive)) {
                DEBUG(("qt_startProcess: \"%s\" is already in BEGINLIBPATH",
                       qPrintable(fullPath)));
            } else {
                DEBUG(("qt_startProcess: prepending \"%s\" to BEGINLIBPATH",
                       qPrintable(fullPath)));
                prependedLibPath = true;
                QByteArray newLibPath = libPathBuf;
                newLibPath.prepend(';').prepend(QFile::encodeName(fullPath));
                DosSetExtLIBPATH(newLibPath, BEGIN_LIBPATH);
            }
        }

        const char *programReal = fullProgramName.data();
        const char **argvReal = argv;

        if (mode == P_SESSION) {
            // @todo P_SESSION isn't supported by kLIBC atm, use DosStartSession
            // below
        } else {
            // get the application type of our program (note that we cannot use
            // DosGetInfoBlocks/PIB because it could be overwritten there by e.g.
            // morphing from VIO to PM which doesn't cancel the fact that we
            // are VIO from the OS/2 loader's POV.
            ULONG flags;
            arc = DosQueryAppType(QFile::encodeName(qAppFileName()), &flags);
            if (arc == NO_ERROR && (flags & 0x7) != FAPPTYP_WINDOWAPI) {
                // we are originally not the PM application and thus DosExecPgm()
                // won't be able to start PM applications directly (note that the
                // other way around it works athough undocumented). Check what
                // the target program is.
                arc = DosQueryAppType(fullProgramName, &flags);
                if (arc == NO_ERROR && (flags & 0x7) == FAPPTYP_WINDOWAPI) {
                    // its PM, we need a proxy
                    // @todo use P_SESSION once it's implemented in kLIBC!
                    // @todo check if FS VIO apps need a proxy
                    programReal = ::getenv("COMSPEC"); // returns static!
                    if (programReal == 0)
                        programReal = "cmd.exe";
                    argvReal = argvBase + 1;
                    argvReal[0] = programReal;
                    argvReal[1] = "/c";
                    argv[0] = fullProgramName.data();
                }
            }
        }

#if defined(QPROCESS_DEBUG)
        DEBUG(("qt_startProcess: executable \"%s\"",
               qPrintable(QFile::decodeName(programReal))));
        for (int i = 0; argvReal[i]; ++i) {
            DEBUG(("qt_startProcess:  arg[%d] \"%s\"",
                   i, qPrintable(QFile::decodeName(argvReal[i]))));
        }
#endif

        // finally, start the thing (note that due to the incorrect definiton of
        // spawnve we have to perform a pointless cast; the devinition for the
        // 3rd and 4th arg should be: char const * const *)
        if (mode == P_SESSION) {
            // @todo P_SESSION isn't supported by kLIBC atm, use DosStartSession
            bool needCmd = false;
            int dot = fullProgramName.lastIndexOf('.');
            Q_ASSERT(dot > 0);
            if (dot > 0)
                needCmd = qstricmp(fullProgramName.data() + dot, ".cmd") == 0 ||
                          qstricmp(fullProgramName.data() + dot, ".bat") == 0;

            QByteArray args;
            foreach(const QString &arg, arguments) {
                if (!args.isEmpty())
                    args += ' ';
                if (!arg.isEmpty())
                    args += '"' + QFile::encodeName(arg) + '"';
            }
            if (needCmd) {
                args.prepend("\" ");
                args.prepend(programReal);
                args.prepend("/C \"");
                programReal = ::getenv("COMSPEC"); // returns static!
                if (programReal == 0)
                    programReal = "cmd.exe";
             }

            QByteArray env;
            if (envv != environ)
                for (char **e = envv; *e; ++e)
                    env += *e + '\0';

#if defined(__INNOTEK_LIBC__)
            // make sure we store arguments in the low memory for
            // DosStartSession() as it cannot work with high memory
            PSZ pgmNameLow = (PSZ)::_lmalloc(::strlen(programReal) + 1);
            ::strcpy(pgmNameLow, programReal);
            PSZ argsLow = (PSZ)::_lmalloc(args.size() + 1);
            ::memcpy(argsLow, args, args.size() + 1);
            PSZ envLow = 0;
            if (!env.isEmpty()) {
                envLow = (PSZ)::_lmalloc(env.size() + 1);
                ::memcpy(envLow, env, env.size() + 1);
            }
#else
            PSZ pgmNameLow = (PSZ)programReal;
            PSZ argsLow = args.data();
            PSZ envLow = env.isEmpty() ? 0 : env.data();
#endif

            STARTDATA data;
            data.Length = sizeof(data);
            data.Related = SSF_RELATED_INDEPENDENT;
            data.FgBg = SSF_FGBG_FORE;
            data.TraceOpt = SSF_TRACEOPT_NONE;
            data.PgmTitle = 0;
            data.PgmName = pgmNameLow;
            data.PgmInputs = argsLow;
            data.TermQ = 0;
            data.Environment = envLow;
            data.InheritOpt = SSF_INHERTOPT_PARENT;
            data.SessionType = SSF_TYPE_DEFAULT;
            data.IconFile = 0;
            data.PgmHandle = 0;
            data.PgmControl = SSF_CONTROL_VISIBLE;
            data.InitXPos = data.InitYPos = data.InitXSize = data.InitYSize = 0;
            data.Reserved = 0;
            data.ObjectBuffer = 0;
            data.ObjectBuffLen = 0;

            ULONG ulSid, ulPidDummy;
            arc = DosStartSession(&data, &ulSid, &ulPidDummy);
            DEBUG(("qt_startProcess: DosStartSession() returned %ld", arc));
            // Note: for SSF_RELATED_INDEPENDENT, PID of the started process is
            // unknown, return 0 to indicate this
            if (arc == NO_ERROR)
                pid = 0;
            else
                pid = -1;

#if defined(__INNOTEK_LIBC__)
            if (envLow)
                ::free(envLow);
            ::free(argsLow);
            ::free(pgmNameLow);
#endif
        } else {
            pid = spawnve(mode, programReal, const_cast<char * const *>(argvReal), envv);
        }

        DEBUG(("qt_startProcess: pid %d", pid));

        if (prependedLibPath) {
            // restore BEGINLIBPATH
            DosSetExtLIBPATH(libPathBuf, BEGIN_LIBPATH);
        }
    }

    // Clean up duplicated memory.
    for (int i = 1 /* 0 is programName */; i <= arguments.count(); ++i)
        delete [] argv[i];
    delete [] argvBase;
    if (envv != environ) {
        for (int i = 0; i < envc; ++i)
            delete [] envv[i];
        delete [] envv;
    }

    // restore the current directory
    QDir::setCurrent(curDir);

    return pid;
}

void QProcessPrivate::startProcess()
{
    Q_Q(QProcess);

    DEBUG(("QProcessPrivate::startProcess"));

    // Initialize pipes
    if (!createChannel(stdinChannel))
        return;
    if (processChannelMode != QProcess::ForwardedChannels) {
        if (!createChannel(stdoutChannel) || !createChannel(stderrChannel))
            return;
    }

    pipeData[InPipe].signaled = false;
    pipeData[OutPipe].signaled = false;
    pipeData[ErrPipe].signaled = false;

    procKey = QProcessManager::addProcess(q);

    if (procKey == QProcessManager::InvalidProcKey) {
        // setErrorString() is called inside addProcess()
        Q_ASSERT(!q->errorString().isEmpty());
        processError = QProcess::FailedToStart;
        emit q->error(processError);
        cleanup();
        return;
    }

    // Start the process (platform dependent)

    q->setProcessState(QProcess::Starting);

    int rc;

    QString error;
    int tmpStdin = -1, tmpStdout = -1, tmpStderr = -1;
    int realStdin = fileno(stdin), realStdout = fileno(stdout),
        realStderr = fileno(stderr);

    do {
        // save & copy the stdin handle
        if ((rc = tmpStdin = dup(realStdin)) != -1) {
            HFILE handle = stdinChannel.pipe.client;
            if (stdinChannel.type == Channel::PipeSink) {
                // process -> process redirection
                if (stdinChannel.pipe.server != HPIPE(~0)) {
                    // we are the server
                    handle = (HFILE)stdinChannel.pipe.server;
                } else {
                    // we are the client, use the server's variable
                    handle = stdinChannel.process->stdoutChannel.pipe.client;
                }
                Q_ASSERT(handle != HFILE(~0));
            }
            Q_ASSERT(_imphandle(handle) != -1);
            rc = dup2(_imphandle(handle), realStdin);
        }
        if (rc == -1) {
            DEBUG(("QProcessPrivate::startProcess: dup/dup2 for stdin "
                   "failed with %d (%s)", errno, strerror(errno)));
            break;
        }
        // save & copy the stdout and stderr handles if asked to
        if (processChannelMode != QProcess::ForwardedChannels) {
            // save & copy the stdout handle
            if ((rc = tmpStdout = dup(realStdout)) != -1) {
                HFILE handle = stdoutChannel.pipe.client;
                if (stdoutChannel.type == Channel::PipeSource) {
                    // process -> process redirection
                    if (stdoutChannel.pipe.server != HPIPE(~0)) {
                        // we are the server
                        handle = (HFILE)stdoutChannel.pipe.server;
                    } else {
                        // we are the client, use the server's variable
                        handle = stdoutChannel.process->stdinChannel.pipe.client;
                    }
                    Q_ASSERT(handle != HFILE(~0));
                }
                Q_ASSERT(_imphandle(handle) != -1);
                rc = dup2(_imphandle(handle), realStdout);
            }
            if (rc == -1) {
                DEBUG(("QProcessPrivate::startProcess: dup/dup2 for stdout "
                       "failed with %d (%s)", errno, strerror(errno)));
                break;
            }
            if ((rc = tmpStderr = dup(realStderr)) != -1) {
                // merge stdout and stderr if asked to
                if (processChannelMode == QProcess::MergedChannels) {
                    rc = dup2(realStdout, realStderr);
                } else {
                    HFILE handle = stderrChannel.pipe.client;
                    Q_ASSERT(_imphandle(handle) != -1);
                    rc = dup2(_imphandle(handle), realStderr);
                }
            }
            if (rc == -1) {
                DEBUG(("QProcessPrivate::startProcess: dup/dup2 for stderr "
                       "failed with %d (%s)", errno, strerror(errno)));
                break;
            }
        }

    } while (false);

    int pid = -1;
    if (rc != -1) {
        QStringList env = environment.toStringList();
        pid = qt_startProcess(program, arguments, workingDirectory, &env);
    }

    // cancel STDIN/OUT/ERR redirections
    if (tmpStdin != -1) {
        dup2(tmpStdin, realStdin);
        close(tmpStdin);
    }
    if (tmpStdout != -1) {
        dup2(tmpStdout, realStdout);
        close(tmpStdout);
    }
    if ( tmpStderr != -1) {
        dup2(tmpStderr, realStderr);
        close(tmpStderr);
    }

    if (rc == -1 || pid == -1) {
        // Cleanup, report error and return
        q->setProcessState(QProcess::NotRunning);
        processError = QProcess::FailedToStart;
        if (rc == -1) {
            // handle duplication failed
            q->setErrorString(QProcess::tr("Process failed to start: %1")
                                           .arg(qt_error_string(errno)));
        } else {
            DEBUG(("spawnvpe failed: %s", qPrintable(qt_error_string(errno))));
            q->setErrorString(QProcess::tr("Process failed to start: %1")
                                           .arg(qt_error_string(errno)));
        }
        emit q->error(processError);
        QProcessManager::removeProcess(procKey);
        procKey = QProcessManager::InvalidProcKey;
        cleanup();
        return;
    }

    this->pid = Q_PID(pid);

    // close the client ends inherited by the started process (it's necessary to
    // make sure that the started process owns the only handle to the client end
    // and when it closes this handle the other party will notice it and e.g.
    // stop waiting for new data).

    if (stdinChannel.type == Channel::PipeSink) {
        // process -> process redirection
        if (stdinChannel.pipe.server != HPIPE(~0)) {
            // we are the server, leave the handle for the other party
        } else {
            // we are the client, close the handle
            DosClose(stdinChannel.process->stdoutChannel.pipe.client);
            stdinChannel.process->stdoutChannel.pipe.client = HFILE(~0);
        }
    } else {
        if (stdinChannel.pipe.client != HFILE(~0)) {
            DosClose(stdinChannel.pipe.client);
            stdinChannel.pipe.client = HFILE(~0);
        }
    }
    if (stdoutChannel.type == Channel::PipeSource) {
        // process -> process redirection
        if (stdoutChannel.pipe.server != HPIPE(~0)) {
            // we are the server, leave the handle for the other party
        } else {
            // we are the client, close the handle
            DosClose(stdoutChannel.process->stdinChannel.pipe.client);
            stdoutChannel.process->stdinChannel.pipe.client = HFILE(~0);
        }
    } else {
        if (stdoutChannel.pipe.client != HFILE(~0)) {
            DosClose(stdoutChannel.pipe.client);
            stdoutChannel.pipe.client = HFILE(~0);
        }
    }
    if (stderrChannel.pipe.client != HFILE(~0)) {
        DosClose(stderrChannel.pipe.client);
        stderrChannel.pipe.client = HFILE(~0);
    }

    // give the process a chance to start ...
    DosSleep(100);

    _q_startupNotification();
}

bool QProcessPrivate::processStarted()
{
    // we don't actually wait for any notification from the child process
    // assuming it has been started as long as spawnvpe() returns success
    return processState == QProcess::Starting;
}

// static
qint64 QProcessPrivate::bytesAvailableFromPipe(HPIPE hpipe, bool *closed)
{
    qint64 bytes = 0;
    if (hpipe != HPIPE(~0)) {
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

qint64 QProcessPrivate::bytesAvailableFromStdout() const
{
    qint64 bytes = 0;
    if (!dying) {
        Q_ASSERT(pipeData[OutPipe].signaled);
        // reuse the number we got in _q_notified()
        bytes = pipeData[OutPipe].bytes;
    } else {
        if (stdoutChannel.type == QProcessPrivate::Channel::Normal)
            bytes = bytesAvailableFromPipe(stdoutChannel.pipe.server);
    }

    DEBUG(("QProcessPrivate::bytesAvailableFromStdout() == %lld", bytes));
    return bytes;
}

qint64 QProcessPrivate::bytesAvailableFromStderr() const
{
    qint64 bytes = 0;
    if (!dying) {
        Q_ASSERT(pipeData[ErrPipe].signaled);
        // reuse the number we got in _q_notified()
        bytes = pipeData[ErrPipe].bytes;
    } else {
        if (stderrChannel.type == QProcessPrivate::Channel::Normal)
            bytes = bytesAvailableFromPipe(stderrChannel.pipe.server);
    }

    DEBUG(("QProcessPrivate::bytesAvailableFromStderr() == %lld", bytes));
    return bytes;
}

qint64 QProcessPrivate::readFromStdout(char *data, qint64 maxlen)
{
    ULONG actual = 0;
    APIRET arc = DosRead(stdoutChannel.pipe.server, data, maxlen, &actual);

    qint64 bytesRead = -1;
    if (arc == NO_ERROR) {
        bytesRead = (qint64)actual;
    }

    DEBUG(("QProcessPrivate::readFromStdout(%p \"%s\", %lld) == %lld",
           data, qt_prettyDebug(data, bytesRead, 16).constData(), maxlen, bytesRead));
    return bytesRead;
}

qint64 QProcessPrivate::readFromStderr(char *data, qint64 maxlen)
{
    ULONG actual = 0;
    APIRET arc = DosRead(stderrChannel.pipe.server, data, maxlen, &actual);

    qint64 bytesRead = -1;
    if (arc == NO_ERROR) {
        bytesRead = (qint64)actual;
    }

    DEBUG(("QProcessPrivate::readFromStderr(%p \"%s\", %lld) == %lld",
           data, qt_prettyDebug(data, bytesRead, 16).constData(), maxlen, bytesRead));
    return bytesRead;
}

void QProcessPrivate::tryCloseStdinPipe()
{
    Q_ASSERT(stdinChannel.pipe.closePending);

    if (stdinChannel.pipe.closePending) {
        // check if the other end has read everything from the pipe buf so that
        // it's safe to close it (satisfy the closeWriteChannel() request) now
        if (pipeData[InPipe].bytes == PIPE_SIZE_STDIN) {
            destroyPipe(stdinChannel.pipe);
        }
    }
}

qint64 QProcessPrivate::writeToStdin(const char *data, qint64 maxlen)
{
    QMutexLocker lock(QProcessManager::pipeStateLock());

    // Reset the number of bytes before writing to the pipe. This makes sure
    // this method will be called again only after QProcessManager::run() sets
    // bytes to non-zero and sends us a signal. Note that we do it under the
    // lock to avoid cases when ProcessManager::run() overwrites this bytes
    // field with an outdated value from the previous pipe event (that could get
    // reported again due to another event fired on behalf of some other pipe).
    pipeData[InPipe].bytes = 0;

    ULONG actual = 0;
    APIRET arc = DosWrite(stdinChannel.pipe.server, data, maxlen, &actual);

    qint64 written = -1;
    if (arc == NO_ERROR) {
        written = (qint64)actual;
    }

    DEBUG(("QProcessPrivate::writeToStdin(%p \"%s\", %lld) == %lld",
           data, qt_prettyDebug(data, maxlen, 16).constData(), maxlen, written));
    return written;
}

void QProcessPrivate::terminateProcess()
{
    DEBUG(("QProcessPrivate::terminateProcess()"));
    if (pid) {
        HSWITCH hswitch = WinQuerySwitchHandle(NULL, pid);
        if (hswitch != NULLHANDLE) {
            SWCNTRL swcntrl = { 0 };
            APIRET rc = WinQuerySwitchEntry(hswitch,  &swcntrl);
            // WinQuerySwitchEntry will return a switch entry of the parent
            // process if the specfied one doesn't have a separate session
            // (running a plain CMD.EXE is an example); ignore this case.
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
            }
        }
    }
}

void QProcessPrivate::killProcess()
{
    DEBUG(("QProcessPrivate::killProcess()"));
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

bool QProcessPrivate::waitForStarted(int msecs)
{
    Q_Q(QProcess);

    if (processState == QProcess::Running)
        return true;

    if (processError == QProcess::FailedToStart)
        return false;

    processError = QProcess::Timedout;
    q->setErrorString(QProcess::tr("Process operation timed out"));
    return false;
}

bool QProcessPrivate::waitFor(WaitCond cond, int msecs)
{
    Q_Q(QProcess);

#if defined QPROCESS_DEBUG
    const char *condStr = cond == WaitReadyRead ? "ReadyRead" :
                          cond == WaitBytesWritten ? "BytesWritten" :
                          cond == WaitFinished ? "Finished" : "???";
    DEBUG(("QProcessPrivate::waitFor(%s, %d)", condStr, msecs));
#endif

    QTime stopWatch;
    stopWatch.start();

    APIRET arc;
    bool ret = false;

    ensureWaitSem();
    waitMode = true;

    // QProcessManager::run() could post a method invocation before noticing we
    // entered waitMode, process it now to avoid an endless hang in wait state
    // due to the absense of the notification via the semaphore
    bool firstTime = true;
    QCoreApplication::sendPostedEvents(q, QEvent::MetaCall);
    if (QCoreApplication::instance() == NULL) {
        // however, if there is no QApplication, _q_notified() won't be called
        // by the above, only removed from the queue. So we need a manual call.
        firstTime = false;
    }

    forever {
        if (firstTime) {
            firstTime = false;
        } else {
            // check all conditions upon the signal from QProcessManager::run()
            _q_notified(CanAll);
        }

        bool done = false;

        switch (cond)
        {
            case WaitReadyRead: {
                // check if there was a _q_canReadStandardOutput/Error() signal
                // that got something from the pipe
                if (processChannel == QProcess::StandardOutput &&
                    pipeData[OutPipe].signaled) {
                    ret = pipeData[OutPipe].result;
                    done = true;
                    break;
                }
                if (processChannel == QProcess::StandardError &&
                    pipeData[ErrPipe].signaled) {
                    ret = pipeData[ErrPipe].result;
                    done = true;
                    break;
                }

                // check if there was a _q_processDied() signal
                if (dying || processState == QProcess::NotRunning) {
                    done = true;
                    break;
                }

                break;
            }

            case WaitBytesWritten: {
                // check if there was a _q_canWrite() signal that wrote
                // something to the pipe
                if (pipeData[InPipe].signaled) {
                    ret = pipeData[InPipe].result;
                    done = true;
                    break;
                }

                // check if there was a _q_processDied() signal
                if (dying || processState == QProcess::NotRunning) {
                    done = true;
                    break;
                }

                if (writeBuffer.isEmpty()) {
                    done = true;
                    break;
                }

                break;
            }

            case WaitFinished: {
                // check if there was a _q_processDied() signal
                if (dying || processState == QProcess::NotRunning) {
                    ret = true;
                    done = true;
                    break;
                }

                break;
            }
        }

        // reset all signaled flags
        pipeData[OutPipe].signaled = false;
        pipeData[ErrPipe].signaled = false;
        pipeData[InPipe].signaled = false;

        if (done)
            break;

        // wait for the new signals
        int timeout = qt_timeout_value(msecs, stopWatch.elapsed());
        qDosNI(arc = DosWaitEventSem(waitSem, (ULONG)timeout));

        if (arc == ERROR_TIMEOUT) {
            processError = QProcess::Timedout;
            q->setErrorString(QProcess::tr("Process operation timed out"));
            break;
        } else if (arc != NO_ERROR) {
            Q_ASSERT(arc == NO_ERROR);
            break;
        }
    }

    waitMode = false;

    ULONG postCnt = 0;
    arc = DosResetEventSem(waitSem, &postCnt);
    if (arc == NO_ERROR && postCnt) {
        // QProcessManager::run() posted the semaphore before seeing that we
        // left waitMode, repost it as a method invocation to avoid signal loss
        QMetaObject::invokeMethod(q, "_q_notified", Qt::QueuedConnection,
                                  Q_ARG(int, QProcessPrivate::CanAll));
    }

    DEBUG(("QProcessPrivate::waitFor(%s, %d) returns %d", condStr, msecs, ret));
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

bool QProcessPrivate::waitForWrite(int msecs)
{
    // ### this function isn't actually used in OS/2 and Unix code paths
    return false;
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
    // check if our process is dead
    int exitStatus;
    pid_t waitResult = waitpid(pid, &exitStatus, WNOHANG);
    if (waitResult > 0) {
        crashed = !WIFEXITED(exitStatus);
        exitCode = WEXITSTATUS(exitStatus);
        DEBUG(() << "QProcessPrivate::waitForDeadChild() dead with exitCode"
                 << exitCode << ", crashed?" << crashed);
        return true;
    }
#if defined QPROCESS_DEBUG
    DEBUG(() << "QProcessPrivate::waitForDeadChild() not dead!");
    if (waitResult == -1)
        DEBUG(() << "QProcessPrivate::waitForDeadChild()" << strerror(errno));
#endif
    return false;
}

void QProcessPrivate::_q_notified(int flags)
{
    DEBUG(("QProcessPrivate::_q_notified: flags %x", flags));

    // note: in all read cases below, we look for the number of bytes actually
    // available to sort out (ignore) outdated notifications from
    // QProcessManager::run() indicated by 0 bytes and to detect closures of
    // the remote end of the pipe indicated by bytes < 0

    // note 2: the PipeData::signaled flag may be set when we enter this method
    // which means that waitFor() found more than one posted _q_notified() call
    // of the same type in the event queue; we need to gracefully handle them
    // all (they may inform about separate payloads)

    if (flags & CanReadStdOut) {
        Q_ASSERT(!pipeData[OutPipe].signaled || waitMode);
        bool closed = false;
        qint64 bytes = bytesAvailableFromPipe(stdoutChannel.pipe.server, &closed);
        if (bytes || closed) {
            pipeData[OutPipe].bytes = bytes;
            pipeData[OutPipe].signaled = true;
            pipeData[OutPipe].result = _q_canReadStandardOutput();
            if (closed && bytes) {
                // ask _q_canReadStandardOutput() to close the pipe by setting
                // bytes to 0 (only if not already done by the previous call)
                pipeData[OutPipe].bytes = 0;
                _q_canReadStandardOutput();
            }
            if (!waitMode) {
                // reset the signaled flag
                pipeData[OutPipe].signaled = false;
            }
#if defined QPROCESS_DEBUG
        } else if (!waitMode) {
            DEBUG(("QProcessPrivate::_q_notified: stale CanReadStdOut signal!"));
#endif
        }
    }

    if (flags & CanReadStdErr) {
        Q_ASSERT(!pipeData[ErrPipe].signaled || waitMode);
        bool closed = false;
        qint64 bytes = bytesAvailableFromPipe(stderrChannel.pipe.server, &closed);
        if (bytes || closed) {
            pipeData[ErrPipe].bytes = bytes;
            pipeData[ErrPipe].signaled = true;
            pipeData[ErrPipe].result = _q_canReadStandardError();
            if (closed && bytes) {
                // ask _q_canReadStandardError() to close the pipe by setting
                // bytes to 0 (only if not already done by the previous call)
                pipeData[ErrPipe].bytes = 0;
                _q_canReadStandardError();
            }
            if (!waitMode) {
                // reset the signaled flag
                pipeData[ErrPipe].signaled = false;
            }
#if defined QPROCESS_DEBUG
        } else if (!waitMode) {
            DEBUG(("QProcessPrivate::_q_notified: stale CanReadStdErr signal!"));
#endif
        }
    }

    if (flags & CanWrite) {
        if (pipeData[InPipe].bytes) {
            pipeData[InPipe].signaled = true;
            pipeData[InPipe].result = _q_canWrite();
            if (!waitMode) {
                // reset the signaled flag
                pipeData[InPipe].signaled = false;
            }
        }
    }

    if (flags & CanDie) {
        _q_processDied();
    }
}

bool QProcessPrivate::startDetached(const QString &program, const QStringList &arguments,
                                    const QString &workingDirectory, qint64 *pid)
{
    int startedPid = qt_startProcess(program, arguments, workingDirectory,
                                     NULL, true /* detached */);

    if (startedPid == -1)
        return false;

    if (pid)
        *pid = startedPid;
    return true;
}

QT_END_NAMESPACE

#endif // QT_NO_PROCESS
