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

#include "qos2window.h"
#include "qos2screen.h"

#include <qpa/qwindowsysteminterface.h>

#include <QtGui/QGuiApplication>
#include <QtEventDispatcherSupport/private/qos2guieventdispatcher_p.h>

QT_BEGIN_NAMESPACE

namespace {

enum {
    DefaultWindowWidth = 160,
    DefaultWindowHeight = 160,
};

enum {
    WinData_QOS2Window = QWL_USER,
    WinDataSize = WinData_QOS2Window + 4, // Must always be last_value + 4!
};

const bool lcQpaMessagesDebug = lcQpaMessages().isDebugEnabled();

PFNWP QtOldFrameProc = nullptr;

MRESULT EXPENTRY QtFrameProc(HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
    // NOTE: PM keeps bombing WC_FRAME with WM_QUERYICON every 0.5s for no reason, ignore it.
    if (Q_UNLIKELY(lcQpaMessagesDebug) && msg != WM_QUERYICON)
        qCDebug(lcQpaMessages, "MESSAGE: hwnd %08lX msg %s (%04lX) mp1 0x%08lX mp2 0x%08lX",
                hwnd, QOS2GuiEventDispatcher::messageName(msg), msg, (ULONG)mp1, (ULONG)mp2);

    QOS2Window *that = nullptr;

    // Get the QOS2Window pointer.
    HWND hwndClient = WinWindowFromID(hwnd, FID_CLIENT);
    Q_ASSERT(hwndClient);
    if (hwndClient)
        that = static_cast <QOS2Window *> (WinQueryWindowPtr(hwndClient, WinData_QOS2Window));

    // WinData_QOS2Window is null during widnow creation, ignore this case for now.
    if (that) {
        Q_ASSERT(that->hwnd() == hwndClient);
        switch (msg) {
        default: break;
        }
    }

    MRESULT mrc = QtOldFrameProc(hwnd, msg, mp1, mp2);

    if (that) {
        switch (msg) {
        case WM_WINDOWPOSCHANGED: {
            // QtOldFrameProc has already resized FID_CLIENT here, inform
            // QOS2Window. We prefer this to individual WM_SIZE/WM_MOVE to
            // compress simultaneous changes into one Qt-level notification.
            PSWP pswp = (PSWP)PVOIDFROMMP(mp1);
            if (pswp->fl & (SWP_SIZE | SWP_MOVE))
                that->handleSizeMove();
            break;
        }
        default: break;
        }
    }

    return mrc;
}

MRESULT EXPENTRY QtWindowProc(HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
    if (Q_UNLIKELY(lcQpaMessagesDebug))
        qCDebug(lcQpaMessages, "MESSAGE: hwnd %08lX msg %s (%04lX) mp1 0x%08lX mp2 0x%08lX",
                hwnd, QOS2GuiEventDispatcher::messageName(msg), msg, (ULONG)mp1, (ULONG)mp2);

    // Get the QOS2Window pointer.
    QOS2Window *that = static_cast <QOS2Window *> (WinQueryWindowPtr(hwnd, WinData_QOS2Window));

    // WinData_QOS2Window is null during widnow creation, ignore this case for now.
    if (that) {
        Q_ASSERT(that->hwnd() == hwnd);
        switch (msg) {
        case WM_ACTIVATE: that->handleWmActivate(mp1); return 0;
        case WM_PAINT: that->handleWmPaint(); return 0;
        case WM_WINDOWPOSCHANGED: {
            // Handle size/move changes if not already done in QtFrameProc.
            if (!that->hwndFrame() || that->hwndFrame() == that->hwnd())
                that->handleSizeMove();
            break;
        }
        default: break;
        }
    }

    return WinDefWindowProc(hwnd, msg, mp1, mp2);
}

} // unnamed namespace

QOS2Window::QOS2Window(QWindow *window)
    : QPlatformWindow(window)
{
    const QRect rect = initialGeometry(window, window->geometry(), DefaultWindowWidth, DefaultWindowHeight);

    const Qt::WindowType type = window->type();
    Qt::WindowFlags flags = window->flags();
    QString title = window->title();

    if (type == Qt::Desktop) {
        mHwnd = WinQueryDesktopWindow(NULLHANDLE, NULLHANDLE);
        return;
    }

    const bool isTopLevel = window->isTopLevel();
    const bool isPopup = type == Qt::Popup || type == Qt::ToolTip;
    const bool isDialog = ((type == Qt::Dialog) || (type == Qt::Sheet) || (type == Qt::MSWindowsFixedSizeDialogHint));
    const bool isTool = ((type == Qt::Tool) || (type == Qt::Drawer));

    const QWindow *parent = isTopLevel ? window->transientParent() : window->parent();
    const QOS2Window *os2parent = parent ? static_cast<QOS2Window *>(parent->handle()) : nullptr;
    Q_ASSERT(!parent || os2parent);

    qCInfo(lcQpaWindows) << window << DV(isTopLevel) << type << flags << rect << DV(title) << DV(parent);

    const char *className;

    if (isPopup || type == Qt::ToolTip) {
        className = "Qt5.QPopup";
        static bool IsClassNameRegistered = false;
        if (Q_UNLIKELY(!IsClassNameRegistered))
            WinRegisterClass(NULLHANDLE, className, QtWindowProc, CS_SAVEBITS, WinDataSize);

    } else {
        className = "Qt5.QWindow";
        static bool IsClassNameRegistered = false;
        if (Q_UNLIKELY(!IsClassNameRegistered))
            WinRegisterClass(NULLHANDLE, className, QtWindowProc, 0, WinDataSize);
    }

    HWND hwndOwner = os2parent ? os2parent->mainHwnd() : NULLHANDLE;

    ULONG style = WS_CLIPSIBLINGS | WS_CLIPCHILDREN;

    LONG x = rect.x(), y = rect.y(), cx = rect.width(), cy = rect.height();

    // Flip y coordinate.
    y = (isTopLevel ? QOS2Screen::Height() : parent->height()) - (y + cy);

    // Fix top level window flags in case only the type flags are passed (taken from qwindowswindow.cpp).
    if (isTopLevel) {
        switch (flags) {
        case Qt::Window:
            flags |= Qt::WindowTitleHint | Qt::WindowSystemMenuHint | Qt::WindowMinimizeButtonHint
                  |Qt::WindowMaximizeButtonHint|Qt::WindowCloseButtonHint;
            break;
        case Qt::Dialog:
        case Qt::Tool:
            flags |= Qt::WindowTitleHint | Qt::WindowSystemMenuHint | Qt::WindowCloseButtonHint;
            break;
        default:
            break;
        }
        if ((flags & Qt::WindowType_Mask) == Qt::SplashScreen)
            flags |= Qt::FramelessWindowHint;
    }

    if (title.isEmpty() && flags & Qt::WindowTitleHint)
        title = isTopLevel ? qAppName() : window->objectName();

    QByteArray title8bit = title.toLocal8Bit();

    // For all top-level windows except popups we create a WC_FRAME as a parent
    // and owner and store it in mHwndFrame. Note that for popups mHwndFrame
    // will be equal to mHwnd (to unify control over top-level windows). Child
    // windows will have mHwndFrame set to NULLHANDLE to distinguish them.

    if (isTopLevel && !isPopup) {
        ULONG frameStyle = 0;
        ULONG frameFlags = type == Qt::Window ? FCF_TASKLIST : 0;

        if ((type == Qt::Window || isDialog || isTool)) {
            if (!(flags & Qt::FramelessWindowHint)) {
                if (flags & Qt::MSWindowsFixedSizeDialogHint) {
                    frameFlags |= FCF_DLGBORDER;
                } else if (isTool) {
                    // Note: while it's common that top-level tool widgets have
                    // a thiner frame, FCF_BORDER makes it too thin and it even
                    // cannot be resized. So, use FCF_SIZEBORDER too.
                    frameFlags |= FCF_SIZEBORDER;
                } else {
                    frameFlags |= FCF_SIZEBORDER;
                }
            }
            if (flags & Qt::WindowTitleHint)
                frameFlags |= FCF_TITLEBAR;
            if (flags & Qt::WindowSystemMenuHint)
                frameFlags |= FCF_SYSMENU | FCF_CLOSEBUTTON;
            if (flags & Qt::WindowMinimizeButtonHint)
                frameFlags |= FCF_MINBUTTON;
            if (flags & Qt::WindowMaximizeButtonHint)
                frameFlags |= FCF_MAXBUTTON;
        } else {
            frameFlags |= FCF_BORDER;
        }

        frameStyle |= FS_NOMOVEWITHOWNER | FS_NOBYTEALIGN;
        frameStyle |= WS_CLIPSIBLINGS | WS_CLIPCHILDREN;

        FRAMECDATA frameData;
        frameData.cb = sizeof(FRAMECDATA);
        frameData.flCreateFlags = frameFlags;
        frameData.hmodResources = NULLHANDLE;
        frameData.idResources = 0;

        // Check whether a default icon is present in .EXE and use it if so.
        ULONG sz = 0;
        if (DosQueryResourceSize(NULLHANDLE, RT_POINTER, 1, &sz) == 0) {
            frameData.flCreateFlags |= FCF_ICON;
            frameData.idResources = 1;
        }

        // Create the frame window.
        mHwndFrame = WinCreateWindow(HWND_DESKTOP, WC_FRAME, title8bit, frameStyle,
                                     0, 0, 0, 0, hwndOwner, HWND_TOP, 0,
                                     &frameData, nullptr);

        if (mHwndFrame == NULLHANDLE)
            qFatal("WinCreateWindow(WC_FRAME) failed with 0x%08lX", WinGetLastError(0));

        PFNWP oldProc = WinSubclassWindow(mHwndFrame, QtFrameProc);

        // Remember QtOldFrameProc only once: it's the same for all WC_FRAME windows.
        if (!QtOldFrameProc)
            QtOldFrameProc = oldProc;

        // Create the client window.
        mHwnd = WinCreateWindow(mHwndFrame, className, title8bit, style, 0, 0, 0, 0,
                                mHwndFrame, HWND_TOP, FID_CLIENT, nullptr, nullptr);
    } else {
        Q_ASSERT(isPopup || hwndOwner);
        HWND hwndParent = isPopup ? HWND_DESKTOP : hwndOwner;
        mHwnd = WinCreateWindow(hwndParent, className, title8bit, style,
                                x, y, cx, cy, hwndOwner, HWND_TOP, 0, nullptr, nullptr);
        // Set mHwndFrame to mHwnd for top-level windows to simplify control.
        if (isTopLevel)
            mHwndFrame = mHwnd;
    }

    if (mHwnd == NULLHANDLE)
        qFatal("WinCreateWindow failed with 0x%08lX", WinGetLastError(0));

    // It isn't mentioned in PMREF that PM is obliged to initialize window
    // data with zeroes (although seems to), so do it ourselves
    for (LONG i = QWL_USER; i <= WinDataSize - 4; i += 4)
        WinSetWindowULong(mHwnd, i, 0);

    qCInfo(lcQpaWindows) << hex << DV(mHwndFrame) << DV(mHwnd);

    if (mHwndFrame && mHwndFrame != mHwnd) {
        // Position the frame window first time to have the client window
        // resized and grab frame margins (we don't trust SV_CXSIZEBORDER et al
        // as the actual frame size depends on many factors).
        ULONG fl = SWP_SIZE | SWP_MOVE;
        HWND hwndBehind = NULLHANDLE;
        if ((flags & Qt::WindowStaysOnTopHint) || (type == Qt::ToolTip)) {
            hwndBehind = HWND_TOP;
            fl |= SWP_ZORDER;
            if (flags & Qt::WindowStaysOnBottomHint)
                qWarning() << "QWidget: Incompatible window flags: the window "
                              "can't be on top and on bottom at the same time";
        } else if (flags & Qt::WindowStaysOnBottomHint) {
            hwndBehind = HWND_BOTTOM;
            fl |= SWP_ZORDER;
        }
        WinSetWindowPos(mHwndFrame, hwndBehind, x, y, cx, cy, fl);

        SWP fswp, swp;
        WinQueryWindowPos(mHwndFrame, &fswp);
        WinQueryWindowPos(mHwnd, &swp);

        // Note: flip top and bottom.
        mFrameMargins = QMargins(swp.x, fswp.cy - swp.cy - swp.y,
                                 fswp.cx - swp.cx - swp.x, swp.y);

        qCInfo(lcQpaWindows) << DV(mFrameMargins);

        // Associate this instance with mHwnd (note: after preliminary positioning
        // to avoid unwanted WM_SIZE etc. message dispatch in QtWindowProc).
        WinSetWindowPtr(mHwnd, WinData_QOS2Window, this);

        // Position the frame window second time to make the client geometry match the request.
        x -= swp.x;
        y -= swp.y;
        cx += swp.x + mFrameMargins.right();
        cy += swp.y + mFrameMargins.top();
        WinSetWindowPos(mHwndFrame, NULLHANDLE, x, y, cx, cy, SWP_SIZE | SWP_MOVE);
    } else {
        // Associate this instance with mHwnd.
        WinSetWindowPtr(mHwnd, WinData_QOS2Window, this);
    }
}

QOS2Window::~QOS2Window()
{
    qCInfo(lcQpaWindows) << hex << DV(mHwndFrame) << DV(mHwnd);

    // Deassociate mHwnd window from the instnace (we don't need any messages after this point).
    WinSetWindowPtr(mHwnd, WinData_QOS2Window, nullptr);

    // Only destroy top-level windows; children will be implicitly destroyed by their parents.
    if (mHwndFrame != NULLHANDLE)
        WinDestroyWindow(mHwndFrame);
}

void QOS2Window::setGeometry(const QRect &rect)
{
    qCInfo(lcQpaWindows) << DV(rect);

    QPlatformWindow::setGeometry(rect);
}

QMargins QOS2Window::frameMargins() const
{
    return mFrameMargins;
}

void QOS2Window::setVisible(bool visible)
{
    qCInfo(lcQpaWindows) << DV(visible);

    WinShowWindow(mainHwnd(), visible);
}

bool QOS2Window::isExposed() const
{
    return WinIsWindowShowing(mainHwnd());
}

bool QOS2Window::isActive() const
{
    return testFlag(Active);
}

WId QOS2Window::winId() const
{
    return (WId)mHwnd;
}

void QOS2Window::requestActivateWindow()
{
    qCInfo(lcQpaWindows);

    if (mHwndFrame)
        WinSetWindowPos(mHwndFrame, NULLHANDLE, 0, 0, 0, 0, SWP_ACTIVATE);
}

void QOS2Window::setWindowState(Qt::WindowStates state)
{
    qCInfo(lcQpaWindows) << DV(state);
}

void QOS2Window::setWindowFlags(Qt::WindowFlags flags)
{
    qCInfo(lcQpaWindows) << DV(flags);
}

void QOS2Window::setWindowTitle(const QString &title)
{
    qCInfo(lcQpaWindows) << DV(title);
}

void QOS2Window::propagateSizeHints()
{
}

HPS QOS2Window::acquirePs()
{
    if (mHps)
        return mHps;

    return WinGetPS(mHwnd);
}

void QOS2Window::releasePs(HPS hps)
{
    if (hps == mHps)
        return;

    WinReleasePS(hps);
}

void QOS2Window::handleWmActivate(MPARAM mp1)
{
    qCInfo(lcQpaEvents) << LONGFROMMP(mp1);

    if (LONGFROMMP(mp1))
        setFlag(Active);
    else
        clearFlag(Active);
}

void QOS2Window::handleWmPaint()
{
    qCDebug(lcQpaEvents);

    RECTL rcl;
    mHps = WinBeginPaint(mHwnd, NULLHANDLE, &rcl);

    QWindowSystemInterface::handleExposeEvent(window(), QRegion(QOS2::ToQRect(rcl, geometry().height())));

    WinEndPaint(mHps);
    mHps = NULLHANDLE;
}

void QOS2Window::handleSizeMove()
{
    QRect newGeo;

    RECTL rcl;
    WinQueryWindowRect(mHwnd, &rcl);

    if (rcl.xRight > rcl.xLeft && rcl.yTop > rcl.yBottom) {
        // Get screen coordinates.
        WinMapWindowPoints(mHwnd, HWND_DESKTOP, (PPOINTL)&rcl, 2);
        newGeo = QOS2::ToQRect(rcl, QOS2Screen::Height());
    }

    QRect oldGeo = geometry();
    qCInfo(lcQpaEvents) << DV(oldGeo) << DV(newGeo);

    if (oldGeo != newGeo) {
        // If QWindow::handle is nullptr (e.g. when this call originates from our ctor), call
        // QPlatformWindow::setGeometry manually (see QWindowSystemInterface::handleGeometryChange).
        if (Q_UNLIKELY(!window()->handle()))
            QPlatformWindow::setGeometry(newGeo);
        QWindowSystemInterface::handleGeometryChange(window(), newGeo);
    }
}

#ifndef QT_NO_DEBUG_STREAM
QDebug operator<<(QDebug d, const QOS2Window *window)
{
    QDebugStateSaver saver(d);
    d.nospace();
    d <<  "QOS2Window(" << (void *)window;
    if (window)
        d << ',' << hex << window->hwndFrame() << ',' << window->hwnd() << ',' << dec << window->geometry();
    d << ')';
    return d;
}
#endif // QT_NO_DEBUG_STREAM

QT_END_NAMESPACE
