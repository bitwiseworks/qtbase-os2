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

//#define QSYSTEMSEMAPHORE_DEBUG

#include "qsystemsemaphore.h"
#include "qsystemsemaphore_p.h"
#include "qcoreapplication.h"
#include "qfile.h"
#include <qdebug.h>

QT_BEGIN_NAMESPACE

#ifndef QT_NO_SYSTEMSEMAPHORE

QSystemSemaphorePrivate::QSystemSemaphorePrivate() :
    semaphore(NULLHANDLE), error(QSystemSemaphore::NoError)
{
}

void QSystemSemaphorePrivate::setErrorString(APIRET arc, const QString &function)
{
    if (arc == NO_ERROR)
        return;

    switch (arc) {
    case ERROR_TOO_MANY_HANDLES:
    case ERROR_NOT_ENOUGH_MEMORY:
        error = QSystemSemaphore::OutOfResources;
        errorString = QCoreApplication::translate("QSystemSemaphore", "%1: out of resources").arg(function);
        break;
    case ERROR_ACCESS_DENIED:
        error = QSystemSemaphore::PermissionDenied;
        errorString = QCoreApplication::translate("QSystemSemaphore", "%1: permission denied").arg(function);
        break;
    case ERROR_INVALID_NAME:
        error = QSystemSemaphore::KeyError;
        errorString = QCoreApplication::translate("QSystemSemaphore", "%1: invalid key name").arg(function);
        break;
    default:
        errorString = QCoreApplication::translate("QSystemSemaphore", "%1: unknown error %2").arg(function).arg(arc);
        error = QSystemSemaphore::UnknownError;
    }

#if defined QSYSTEMSEMAPHORE_DEBUG
    qDebug() << errorString << "arc" << arc << "key" << key << "fileName" << fileName;
#endif
}

void QSystemSemaphorePrivate::handle(QSystemSemaphore::AccessMode)
{
    // don't allow making handles on empty keys
    if (key.isEmpty())
        return;

    // Create it if it doesn't already exist.
    if (semaphore == NULLHANDLE) {
        QByteArray name = QFile::encodeName(fileName);
        APIRET arc;
        // Try to open the semaphore first.
        forever {
            arc = DosOpenEventSem(name, &semaphore);
            if (arc == ERROR_SEM_NOT_FOUND) {
#if defined QSYSTEMSEMAPHORE_DEBUG
                qDebug("QSystemSemaphore::handle: creating a new sem [%s] (%d)...", name.data(), initialValue);
#endif
                if (initialValue < 0) {
                    arc = ERROR_NOT_ENOUGH_MEMORY; // cause OutOfResources
                    break;
                }
                arc = DosCreateEventSem(name, &semaphore, DC_SEM_SHARED, FALSE);
                if (arc == ERROR_DUPLICATE_NAME) {
                    // The other process was faster, try to open it again
                    continue;
                }
                if (arc == NO_ERROR) {
                    // Set the initial resource count
                    ULONG count = initialValue;
                    for (; count && (arc == NO_ERROR || arc == ERROR_ALREADY_POSTED); --count)
                        arc = DosPostEventSem(semaphore);
                    if (!count)
                        arc = NO_ERROR;
                }
            }
#if defined QSYSTEMSEMAPHORE_DEBUG
            else
                qDebug("QSystemSemaphore::handle: using existing sem [%s] (%d)...", name.data(), initialValue);
#endif
            break;
        }
        if (arc != NO_ERROR) {
            setErrorString(arc, QLatin1String("QSystemSemaphore::handle"));
        }
    }
}

void QSystemSemaphorePrivate::cleanHandle()
{
    if (semaphore != NULLHANDLE) {
#if defined QSYSTEMSEMAPHORE_DEBUG
        qDebug("QSystemSemaphore::closeHandler: closing the sem [%s]...", fileName.toLocal8Bit().data());
#endif
        APIRET arc = DosCloseEventSem(semaphore);
        Q_UNUSED(arc);
#if defined QSYSTEMSEMAPHORE_DEBUG
        if (arc != NO_ERROR)
          qDebug("QSystemSemaphorePrivate::closeHandle: DosCloseEventSem failed with %ld", arc);
#endif
    }
    semaphore = NULLHANDLE;
}

bool QSystemSemaphorePrivate::modifySemaphore(int count)
{
    if (semaphore == NULLHANDLE)
        return false;

#if defined QSYSTEMSEMAPHORE_DEBUG
     qDebug("QSystemSemaphore::modifySemaphore: sem [%s] count %d", fileName.toLocal8Bit().data(), count);
#endif

    if (count > 0) {
        // Set the requested resource count
        APIRET arc = NO_ERROR;
        for (; count && (arc == NO_ERROR || arc == ERROR_ALREADY_POSTED); --count)
            arc = DosPostEventSem(semaphore);
        if (count) {
            setErrorString(arc, QLatin1String("QSystemSemaphore::modifySemaphore"));
            return false;
        }
    } else {
        forever {
            APIRET arc = DosWaitEventSem(semaphore, SEM_INDEFINITE_WAIT);
            if (arc == NO_ERROR) {
                // Reset the sem to block other waiters and get the posted count
                ULONG postedCount = 0;
                arc = DosResetEventSem(semaphore, &postedCount);
                if (arc == ERROR_ALREADY_RESET) {
                    // We are not the one who got the posted count, continue waiting
                    continue;
                }
                if (arc == NO_ERROR) {
                    Q_ASSERT(postedCount);
#if defined QSYSTEMSEMAPHORE_DEBUG
                    qDebug("QSystemSemaphore::modifySemaphore: postedCount %ld", postedCount);
#endif
                    // We are the one who got one resource, restore the remainig count
                    while (--postedCount && (arc == NO_ERROR || arc == ERROR_ALREADY_POSTED))
                        arc = DosPostEventSem(semaphore);
                    // Break the forever loop on success
                    if (!postedCount)
                        break;
                }
            }
            if (arc != NO_ERROR) {
                setErrorString(arc, QLatin1String("QSystemSemaphore::modifySemaphore"));
                return false;
            }
        }
    }

    clearError();
    return true;
}

#endif //QT_NO_SYSTEMSEMAPHORE

QT_END_NAMESPACE
