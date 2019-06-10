/***************************************************************************
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

#ifndef QOS2KEYMAPPER_H
#define QOS2KEYMAPPER_H

#include "qos2context.h"

QT_BEGIN_NAMESPACE

class QKeyEvent;
class QOS2Window;
class QWindow;

/*
    An OS/2 KeyboardLayoutItem has 8 possible states meaningful for Qt:
        1. Unmodified
        2. Shift
        3. Control
        4. Control + Shift
        5. Alt
        6. Alt + Shift
        7. Alt + Control
        8. Alt + Control + Shift
*/
struct KeyboardLayoutItem {
    int qtKey[8][2];    // Can be any Qt::Key_<foo>, or unicode character.
                        // The second index is for national keyboard layouts.
    ULONG layoutIds[2]; // Latin layout ID + National layout ID.
    enum { QtKeySize = sizeof(qtKey) / sizeof(qtKey[0]) };
};

class QOS2KeyMapper
{
    Q_DISABLE_COPY(QOS2KeyMapper)

public:
    explicit QOS2KeyMapper();

    bool translateKeyEvent(QOS2Window *window, HWND hwnd, CHRMSG &chm);

    int extraKeyState() const { return mExtraKeyState; }

    QWindow *keyGrabber() const { return mKeyGrabber; }
    void setKeyGrabber(QWindow *w) { mKeyGrabber = w; }

    Qt::KeyboardModifiers queryKeyboardModifiers();
    QList<int> possibleKeys(const QKeyEvent *e) const;

private:
    void updateKeyMap(HWND hwnd, CHRMSG &chm);

    // State holder for LWIN/RWIN and ALTGr keys
    // (ALTGr is also necessary since OS/2 doesn't report ALTGr as KC_ALT)
    int mExtraKeyState = 0;

    static constexpr const size_t NumKeyboardLayoutItems = 256;
    KeyboardLayoutItem mKeyLayout[NumKeyboardLayoutItems];

    QWindow *mKeyGrabber = nullptr;
};

QT_END_NAMESPACE

#endif
