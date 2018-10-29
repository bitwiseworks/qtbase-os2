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

#include "private/qlockfile_p.h"
#include "private/qfilesystementry_p.h"
#include "qt_os2.h"

#include "QtCore/qfileinfo.h"
#include "QtCore/qdatetime.h"
#include "QtCore/qdebug.h"
#include "QtCore/qthread.h"

#include <share.h>

QT_BEGIN_NAMESPACE

QLockFile::LockError QLockFilePrivate::tryLock_sys()
{
    const QByteArray lockFileName = QFile::encodeName(fileName);

    // When writing, allow others to read.
    // When reading, QFile will allow others to read and write, all good.

    const int fd = sopen(lockFileName.constData(), O_RDWR | O_CREAT | O_EXCL, SH_DENYWR, 0666);
    if (fd < 0) {
        switch (errno) {
        case EEXIST:
            return QLockFile::LockFailedError;
        case EACCES:
        case EROFS:
            return QLockFile::PermissionError;
        default:
            return QLockFile::UnknownError;
        }
    }

    // We hold the lock, continue.
    fileHandle = fd;

    QByteArray fileData = lockFileContents();
    QLockFile::LockError error = QLockFile::NoError;
    if (write(fd, fileData.constData(), fileData.size()) != fileData.size() || fsync(fd) != 0)
        error = QLockFile::UnknownError; // partition full
    return error;
}

bool QLockFilePrivate::removeStaleLock()
{
    // QFile::remove fails on OS/2 if the other process is still using the file, so it's not stale.
    return QFile::remove(fileName);
}

bool QLockFilePrivate::isProcessRunning(qint64 pid, const QString &appname)
{
    const QString processName = processNameByPid(pid);

    if (processName.isNull())
        return false; // PID doesn't exist anymore

    if (!processName.isEmpty() && processName != appname)
        return false; // PID got reused by a different application.

    return true;
}

QString QLockFilePrivate::processNameByPid(qint64 pid)
{
    enum { SysStateSize = 4096 };
    char *sys_state = new char[SysStateSize];

    APIRET arc = DosQuerySysState(QS_PROCESS, 0, pid, 0, sys_state, SysStateSize);
    if (arc == ERROR_INVALID_PROCID) // PID doesn't exist anymore
        return QString();

    // Use an empty string to differentiate from the above on any other failure
    QString name = QLatin1String("");

    if (arc == NO_ERROR) {
        QSPTRREC *pPtrRec = (QSPTRREC *)sys_state;
        QSPREC *pProcRec = pPtrRec->pProcRec;

        Q_ASSERT(pProcRec->RecType == QS_PROCESS);

        char path[CCHMAXPATH];
        arc = DosQueryModuleName(pProcRec->hMte, sizeof(path), path);
        if (arc == NO_ERROR) {
            // DosQueryModuleName returns the uppercased path, try to get the real case
            // (to match QCoreApplication::applicationFilePath() and qAppName())
            char path2[CCHMAXPATH];
            if (_realrealpath(path, path2, sizeof(path2)))
                name = QFile::decodeName(path2);
            else
                name = QFile::decodeName(path);
            // Strip the path and extension (realpath slashes depend on -Z[no-]unix so check for both)
            int i = name.lastIndexOf(QLatin1Char('/'));
            if (i < 0)
                i = name.lastIndexOf(QLatin1Char('\\'));
            if (i >= 0)
                name.remove(0, i + 1);
            i = name.lastIndexOf(QLatin1Char('.'));
            if (i >= 0)
                name.truncate(i);
        } else {
            qWarning("DosQueryModuleName returned %ld", arc);
        }
    } else {
        qWarning("DosQuerySysState returned %ld", arc);
    }

    return name;
}

void QLockFile::unlock()
{
    Q_D(QLockFile);
     if (!d->isLocked)
        return;
     close(d->fileHandle);
     int attempts = 0;
     static const int maxAttempts = 500; // 500ms
     while (!QFile::remove(d->fileName) && ++attempts < maxAttempts) {
         // Someone is reading the lock file right now (on OS/2 this prevents deleting it).
         QThread::msleep(1);
     }
     if (attempts == maxAttempts) {
        qWarning() << "Could not remove our own lock file" << d->fileName << ". Either other users of the lock file are reading it constantly for 500 ms, or we (no longer) have permissions to delete the file";
        // This is bad because other users of this lock file will now have to wait for the stale-lock-timeout...
     }
     d->lockError = QLockFile::NoError;
     d->isLocked = false;
}

QT_END_NAMESPACE
