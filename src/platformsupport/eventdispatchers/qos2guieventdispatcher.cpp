/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** Copyright (C) 2019 bww bitwise works GmbH. OS/2 parts.
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

#include "qt_os2.h"

#include "qos2guieventdispatcher_p.h"

#include <qpa/qwindowsysteminterface.h>

#include <QtCore/QCoreApplication>
#include <QtCore/QDebug>

QT_BEGIN_NAMESPACE

/*!
    \class QOS2GuiEventDispatcher
    \brief Event dispatcher for OS/2 Presentation Manager

    \internal
*/

QOS2GuiEventDispatcher::QOS2GuiEventDispatcher(QObject *parent) :
    QEventDispatcherOS2(parent)
{
    setObjectName(QStringLiteral("QOS2GuiEventDispatcher"));
}

bool QOS2GuiEventDispatcher::processEvents(QEventLoop::ProcessEventsFlags flags)
{
    const bool didSendEvents = QEventDispatcherOS2::processEvents(flags);
    return QWindowSystemInterface::sendWindowSystemEvents(flags) || didSendEvents;
}

bool QOS2GuiEventDispatcher::hasPendingEvents()
{
    return QEventDispatcherOS2::hasPendingEvents() || QWindowSystemInterface::windowSystemEventsQueued();
}

void QOS2GuiEventDispatcher::flush()
{
    if(qApp)
        qApp->sendPostedEvents();
}

// Helpers for printing debug output for WM_* messages.
struct MessageDebugEntry
{
    ULONG message;
    const char *description;
    bool interesting;
};

static const MessageDebugEntry
messageDebugEntries[] = {
    {WM_CREATE, "WM_CREATE", true},
    {WM_PAINT, "WM_PAINT", true},
    {WM_CLOSE, "WM_CLOSE", true},
    {WM_DESTROY, "WM_DESTROY", true},
    {WM_MOVE, "WM_MOVE", true},
    {WM_SIZE, "WM_SIZE", true},
    {WM_CHAR, "WM_CHAR", true},
    {WM_VIOCHAR, "WM_VIOCHAR", true},
    {WM_SYSCOMMAND, "WM_SYSCOMMAND", true},
    {WM_ACTIVATE, "WM_ACTIVATE", true},
    {WM_SETFOCUS, "WM_SETFOCUS", true},
    {WM_ENABLE, "WM_ENABLE", true},
    {WM_WINDOWPOSCHANGED, "WM_WINDOWPOSCHANGED", true},
    {WM_BUTTON1DOWN, "WM_BUTTON1DOWN", true},
    {WM_BUTTON1UP, "WM_BUTTON1UP", true},
    {WM_BUTTON1DBLCLK, "WM_BUTTON1DBLCLK", true},
    {WM_BUTTON2DOWN, "WM_BUTTON2DOWN", true},
    {WM_BUTTON2UP, "WM_BUTTON2UP", true},
    {WM_BUTTON2DBLCLK, "WM_BUTTON2DBLCLK", true},
    {WM_BUTTON3DOWN, "WM_BUTTON3DOWN", true},
    {WM_BUTTON3UP, "WM_BUTTON3UP", true},
    {WM_BUTTON3DBLCLK, "WM_BUTTON3DBLCLK", true},
    {WM_RENDERFMT, "WM_RENDERFMT", true},
    {WM_RENDERALLFMTS, "WM_RENDERALLFMTS", true},
    {WM_DESTROYCLIPBOARD, "WM_DESTROYCLIPBOARD", true},
    {WM_ADJUSTWINDOWPOS, "WM_ADJUSTWINDOWPOS", true},
    {WM_ERASEBACKGROUND, "WM_ERASEBACKGROUND", true},
    {WM_MOUSEMOVE, "WM_MOUSEMOVE", true},
    {WM_HITTEST, "WM_HITTEST", false},
    {WM_DRAWCLIPBOARD, "WM_DRAWCLIPBOARD", true},
    {WM_MOUSEENTER, "WM_MOUSEENTER", true},
    {WM_MOUSELEAVE, "WM_MOUSELEAVE", true},
};

static inline const MessageDebugEntry *messageDebugEntry(ULONG msg)
{
    for (size_t i = 0; i < sizeof(messageDebugEntries)/sizeof(MessageDebugEntry); i++)
        if (messageDebugEntries[i].message == msg)
            return messageDebugEntries + i;
    return 0;
}

const char *QOS2GuiEventDispatcher::messageName(unsigned long msg)
{
    if (const MessageDebugEntry *e = messageDebugEntry(msg))
        return e->description;
    return "Unknown";
}

QT_END_NAMESPACE
