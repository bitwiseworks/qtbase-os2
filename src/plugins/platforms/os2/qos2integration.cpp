/****************************************************************************
**
** Copyright (C) 2019 bww bitwise works GmbH.
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

#include "qos2integration.h"

#include "qos2backingstore.h"
#include "qos2keymapper.h"
#include "qos2screen.h"
#include "qos2theme.h"
#include "qos2window.h"

#if QT_CONFIG(clipboard)
#include "qos2clipboard.h"
#endif

#include <qpa/qwindowsysteminterface.h>

#include <QtEventDispatcherSupport/private/qos2guieventdispatcher_p.h>

#include <QtFontDatabaseSupport/private/qfreetypefontdatabase_p.h>
#if QT_CONFIG(fontconfig)
#  include <QtFontDatabaseSupport/private/qgenericunixfontdatabase_p.h>
#else
#  include <qpa/qplatformfontdatabase.h>
#endif

QT_BEGIN_NAMESPACE

/*!
    \class QOS2Integration
    \brief QPlatformIntegration implementation for OS/2 Presentation Manager.
    \internal
*/

QOS2Integration * QOS2Integration::sInstance = nullptr;

QOS2Integration::QOS2Integration(const QStringList &paramList)
{
    Q_ASSERT(sInstance == nullptr);
    sInstance = this;

    Q_UNUSED(paramList);

    // Make sure WM_PAINT/WM_SIZE etc. are handled synchronously.
    QWindowSystemInterface::setSynchronousWindowSystemEvents(true);

    // Create and notify the system about the primary (and the only) screen.
    mScreen = new QOS2Screen();
    QWindowSystemInterface::handleScreenAdded(mScreen);

#if QT_CONFIG(clipboard)
    mClipboard = new QOS2Clipboard();
#endif

    mKeyMapper = new QOS2KeyMapper();
}

QOS2Integration::~QOS2Integration()
{
    delete mKeyMapper;

#if QT_CONFIG(clipboard)
    delete mClipboard;
#endif

    QWindowSystemInterface::handleScreenRemoved(mScreen);
    delete mFontDatabase;

    sInstance = nullptr;
}

bool QOS2Integration::hasCapability(QPlatformIntegration::Capability cap) const
{
    return QPlatformIntegration::hasCapability(cap);
}

QPlatformWindow *QOS2Integration::createPlatformWindow(QWindow *window) const
{
    return new QOS2Window(window);
}

QPlatformBackingStore *QOS2Integration::createPlatformBackingStore(QWindow *window) const
{
    return new QOS2BackingStore(window);
}

QAbstractEventDispatcher * QOS2Integration::createEventDispatcher() const
{
    return new QOS2GuiEventDispatcher;
}

#if QT_CONFIG(clipboard)
QPlatformClipboard *QOS2Integration::clipboard() const
{
    return mClipboard;
}
#endif

QPlatformFontDatabase *QOS2Integration::fontDatabase() const
{
    if (!mFontDatabase) {
#if QT_CONFIG(fontconfig)
        const_cast <QPlatformFontDatabase * &> (mFontDatabase) = new QGenericUnixFontDatabase;
#else
        mFontDatabase = QPlatformIntegration::fontDatabase();
#endif
    }

    return mFontDatabase;
}

QStringList QOS2Integration::themeNames() const
{
    return QStringList(QLatin1String(QOS2Theme::name()));
}

QPlatformTheme *QOS2Integration::createPlatformTheme(const QString &name) const
{
    if (name == QLatin1String(QOS2Theme::name()))
        return new QOS2Theme;
    return QPlatformIntegration::createPlatformTheme(name);
}

QVariant QOS2Integration::styleHint(QPlatformIntegration::StyleHint hint) const
{
    switch (hint) {
    case QPlatformIntegration::CursorFlashTime:
        if (const LONG timeMS = WinQuerySysValue(HWND_DESKTOP, SV_CURSORRATE))
            return QVariant(timeMS > 0 ? int(timeMS) * 2 : 0);
        break;
    case QPlatformIntegration::MouseDoubleClickInterval:
        if (const LONG ms = WinQuerySysValue(HWND_DESKTOP, SV_DBLCLKTIME))
            return QVariant(int(ms));
        break;
    default:
        break;
    }
    return QPlatformIntegration::styleHint(hint);
}

Qt::KeyboardModifiers QOS2Integration::queryKeyboardModifiers() const
{
    return mKeyMapper->queryKeyboardModifiers();
}

QList<int> QOS2Integration::possibleKeys(const QKeyEvent *e) const
{
    return mKeyMapper->possibleKeys(e);
}

void QOS2Integration::beep() const
{
    WinAlarm(HWND_DESKTOP, WA_WARNING); // For QApplication
}

QT_END_NAMESPACE
