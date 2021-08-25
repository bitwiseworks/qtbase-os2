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

#ifndef QOS2INTEGRATION_H
#define QOS2INTEGRATION_H

#include <qpa/qplatformintegration.h>

QT_BEGIN_NAMESPACE

class QOS2Clipboard;
class QOS2KeyMapper;

class QOS2Integration : public QPlatformIntegration
{
public:
    explicit QOS2Integration(const QStringList &paramList);
    virtual ~QOS2Integration();

    bool hasCapability(QPlatformIntegration::Capability cap) const override;

    QPlatformWindow *createPlatformWindow(QWindow *window) const override;
    QPlatformBackingStore *createPlatformBackingStore(QWindow *window) const override;
    QAbstractEventDispatcher *createEventDispatcher() const override;

#if QT_CONFIG(clipboard)
    QPlatformClipboard *clipboard() const override;
#endif

    QPlatformFontDatabase *fontDatabase() const override;

    QStringList themeNames() const override;
    QPlatformTheme *createPlatformTheme(const QString &name) const override;

    QVariant styleHint(StyleHint hint) const override;

    Qt::KeyboardModifiers queryKeyboardModifiers() const override;
    QList<int> possibleKeys(const QKeyEvent *e) const override;

    void beep() const override;

    // OS/2 specifics

    QOS2KeyMapper *keyMapper() const { return mKeyMapper; }

    static QOS2Integration *instance() { return sInstance; }

private:
    QPlatformScreen *mScreen = nullptr;
    QPlatformFontDatabase *mFontDatabase = nullptr;
#if QT_CONFIG(clipboard)
    QOS2Clipboard *mClipboard = nullptr;
#endif

    QOS2KeyMapper *mKeyMapper = nullptr;

    static QOS2Integration * sInstance;
};

QT_END_NAMESPACE

#endif
