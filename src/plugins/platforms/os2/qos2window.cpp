/***************************************************************************
**
** Copyright (C) 2015 Klarälvdalens Datakonsult AB, a KDAB Group company, info@kdab.com, author Tobias Koenig <tobias.koenig@kdab.com>
** Contact: https://www.qt.io/licensing/
**
** This file is part of the plugins of the Qt Toolkit.
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

#include "qos2window.h"

QT_BEGIN_NAMESPACE

QOS2Window::QOS2Window(QWindow *window)
    : QPlatformWindow(window)
{
}

QOS2Window::~QOS2Window()
{
}

void QOS2Window::setGeometry(const QRect &rect)
{
    QPlatformWindow::setGeometry(rect);
}

QMargins QOS2Window::frameMargins() const
{
    return QMargins();
}

void QOS2Window::setVisible(bool /*visible*/)
{
}

bool QOS2Window::isExposed() const
{
    return false;
}

bool QOS2Window::isActive() const
{
    return false;
}

WId QOS2Window::winId() const
{
    return (WId)1;
}

void QOS2Window::requestActivateWindow()
{
}

void QOS2Window::setWindowState(Qt::WindowStates /*state*/)
{
}

void QOS2Window::setWindowFlags(Qt::WindowFlags /*flags*/)
{
}

void QOS2Window::setWindowTitle(const QString &/*title*/)
{
}

void QOS2Window::propagateSizeHints()
{
}

QT_END_NAMESPACE
