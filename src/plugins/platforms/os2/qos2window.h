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

#ifndef QOS2WINDOW_H
#define QOS2WINDOW_H

#include "qos2context.h"

#include <qpa/qplatformwindow.h>

#include <QtCore/QPointer>

QT_BEGIN_NAMESPACE

class QOS2Window : public QPlatformWindow
{
public:
    enum Flags
    {
        Active = 0x1,
        AutoMouseCapture = 0x2,
        BlockedByModal = 0x4,
        InInit = 0x8,
        InSetWindowState = 0x10,
    };

    explicit QOS2Window(QWindow *window);
    virtual ~QOS2Window();

    void initialize() override;

    void setGeometry(const QRect &rect) override;
    QMargins frameMargins() const override;
    void setVisible(bool visible) override;

    bool isExposed() const override;
    bool isActive() const override;

    WId winId() const override;

    void requestActivateWindow() override;
    void setWindowState(Qt::WindowStates state) override;
    void setWindowFlags(Qt::WindowFlags flags) override;

    void setWindowTitle(const QString &title) override;

    void raise() override;
    void lower() override;

    bool setKeyboardGrabEnabled(bool grab) override;
    bool setMouseGrabEnabled(bool grab) override;
    bool hasMouseCapture() const { return WinQueryCapture(HWND_DESKTOP) == mHwnd; }

    bool windowEvent(QEvent *event) override;

    void propagateSizeHints() override;

    inline bool testFlag(unsigned f) const { return (mFlags & f) != 0; }
    inline void setFlag(unsigned f) { mFlags |= f; }
    inline void clearFlag(unsigned f) { mFlags &= ~f; }

    inline HWND hwndFrame() const { return mHwndFrame; }
    inline HWND hwnd() const { return mHwnd; }

    inline HWND mainHwnd() const { return mHwndFrame ? mHwndFrame : mHwnd; }

    HPS acquirePs();
    void releasePs(HPS hps);

    void handleWmClose();
    void handleWmActivate(MPARAM mp1);
    void handleWmSetFocus(MPARAM mp1, MPARAM mp2);
    bool handleWmPaint();
    void handleWmAdjustWindowPos(MPARAM mp1);
    void handleWmWindowPosChanged(MPARAM mp1);
    void handleWmMinMaxFrame(MPARAM mp1);
    void handleMouse(ULONG msg, MPARAM mp1, MPARAM mp2);
    void handleWheel(ULONG msg, MPARAM mp1, MPARAM mp2);
    bool handleWmChar(MPARAM mp1, MPARAM mp2);
    bool handleWmTranslateAccel(MPARAM mp1, MPARAM mp2);

    static QOS2Window *PlatformWindow(HWND hwnd) { return sKnownWindows.value(hwnd); }

private:
    unsigned mFlags = 0;

    HWND mHwnd = NULLHANDLE;
    HPS mHps = NULLHANDLE;

    HWND mHwndFrame = NULLHANDLE;
    QMargins mFrameMargins;
    HSWITCH mSwEntry = NULLHANDLE;
    USHORT normX = 0, normY = 0, normCX = 0, normCY = 0;

    static QHash<HWND, QOS2Window *> sKnownWindows;

    static QPointer<QWindow> sWindowUnderMouse;
    static QPointer<QWindow> sTrackedWindow;
    static QWindow *sPreviousCaptureWindow;
};

QT_END_NAMESPACE

#endif
