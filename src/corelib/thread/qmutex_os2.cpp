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
#include "qmutex.h"
#include "qmutex_p.h"

QT_BEGIN_NAMESPACE

QMutexPrivate::QMutexPrivate()
{
    APIRET arc = DosCreateEventSem(NULL, &event, DCE_POSTONE, FALSE);
    if (arc)
        qWarning("QMutexPrivate::QMutexPrivate: DosCreateEventSem returned %ld", arc);
}

QMutexPrivate::~QMutexPrivate()
{
    DosCloseEventSem(event);
}

bool QMutexPrivate::wait(int timeout)
{
    APIRET arc;
    qDosNI(arc = DosWaitEventSem(event, timeout < 0 ? SEM_INDEFINITE_WAIT : timeout));
    return !arc;
}

void QMutexPrivate::wakeUp() Q_DECL_NOTHROW
{
    DosPostEventSem(event);
}

QT_END_NAMESPACE
