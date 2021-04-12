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
#include "qos2integration.h"
#include "qos2keymapper.h"
#include "qos2screen.h"

#include <qpa/qwindowsysteminterface.h>

#include <QtGui/QGuiApplication>
#include <QtEventDispatcherSupport/private/qos2guieventdispatcher_p.h>

#include <private/qwindow_p.h>

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
        that = static_cast<QOS2Window *>(WinQueryWindowPtr(hwndClient, WinData_QOS2Window));

    // WinData_QOS2Window is null during widnow creation, ignore this case for now.
    if (that) {
        Q_ASSERT(that->hwnd() == hwndClient);
        switch (msg) {
        default: break;
        }
    }

    if (that) {
        switch (msg) {
        case WM_ADJUSTWINDOWPOS: that->handleWmAdjustWindowPos(mp1); break;
        default: break;
        }
    }

    MRESULT mrc = QtOldFrameProc(hwnd, msg, mp1, mp2);

    if (that) {
        switch (msg) {
        // QtOldFrameProc has already resized FID_CLIENT here, inform QOS2Window. We prefer this to
        // individual WM_SIZE/WM_MOVE to compress simultaneous changes into one Qt notification.
        // Note that our class doesn't have CS_MOVE, so we don't get move events in QtWindowProc.
        case WM_WINDOWPOSCHANGED: that->handleWmWindowPosChanged(mp1); break;
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
    QOS2Window *that = static_cast<QOS2Window *>(WinQueryWindowPtr(hwnd, WinData_QOS2Window));

    // WinData_QOS2Window is null during widnow creation, ignore this case for now.
    if (that) {
        Q_ASSERT(that->hwnd() == hwnd);
        const bool hasNoFrame = !that->hwndFrame() || that->hwndFrame() == that->hwnd();

        if ((msg >= WM_MOUSEFIRST && msg <= WM_MOUSELAST) || msg == WM_MOUSELEAVE) {
            if (msg == WM_MOUSELEAVE && hwnd != (HWND)mp1) {
                // This must be a LEAVE message (mp1 = hwndFrom, mp2 = hwndTo) from one of the
                // frame controls forwarded by WC_FRAME to FID_CLIENT. Ignore it as it doesn't
                // actually belong to the given hwnd.
            } else {
                that->handleMouse(msg, mp1, mp2);
            }
            // NOTE: pass mouse events to WinDefWindowProc to cause window activation on click etc.
        } else {
            switch (msg) {
            case WM_CLOSE: that->handleWmClose(); return 0;
            case WM_ACTIVATE: that->handleWmActivate(mp1); break;
            case WM_SETFOCUS: that->handleWmSetFocus(mp1, mp2); break;
            case WM_PAINT: if (that->handleWmPaint()) return 0; else break;
            // Handle size/move changes if not already done in QtFrameProc.
            case WM_WINDOWPOSCHANGED: if (hasNoFrame) that->handleWmWindowPosChanged(mp1); break;
            case WM_MINMAXFRAME: that->handleWmMinMaxFrame(mp1); break;
            case WM_CALCVALIDRECTS: {
                // We must always return this value here to cause PM to reposition
                // our children accordingly (othwerwise we would have to do it
                // ourselves to keep them top-left aligned).
                return (MRESULT)(CVR_ALIGNLEFT | CVR_ALIGNTOP);
            }
            case WM_VSCROLL:
            case WM_HSCROLL:
                that->handleWheel(msg, mp1, mp2); return 0;
            case WM_CHAR:
                if (that->handleWmChar(mp1, mp2)) return (MRESULT)TRUE;
                break;
            case WM_TRANSLATEACCEL: return (MRESULT)that->handleWmTranslateAccel(mp1, mp2);
            default: break;
            }
        }
    }

    return WinDefWindowProc(hwnd, msg, mp1, mp2);
}

PFNWP QtOldSysMenuProc;

MRESULT EXPENTRY QtSysMenuProc(HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
    if (msg == WM_MENUEND) {
        // The pull-down menu is closed, always dismiss the system menu itself.
        WinPostMsg(hwnd, MM_ENDMENUMODE, MPFROMSHORT(TRUE), 0);
    }
    return QtOldSysMenuProc(hwnd, msg, mp1, mp2);
}

void RemoveSysMenuAccels(HWND hwndFrame)
{
    HWND sysMenu = WinWindowFromID(hwndFrame, FID_SYSMENU);
    if (!sysMenu)
        return;

    SHORT subId = SHORT1FROMMR(WinSendMsg(sysMenu, MM_ITEMIDFROMPOSITION, 0, 0));
    if (subId != MIT_ERROR) {
        MENUITEM item;
        WinSendMsg(sysMenu, MM_QUERYITEM, MPFROM2SHORT(subId, FALSE), MPFROMP(&item));
        HWND subMenu = item.hwndSubMenu;
        if (subMenu) {
            USHORT cnt = SHORT1FROMMR(WinSendMsg(subMenu, MM_QUERYITEMCOUNT, 0, 0));
            for (int i = 0; i < cnt; i++) {
                USHORT id = SHORT1FROMMR(WinSendMsg(subMenu, MM_ITEMIDFROMPOSITION, MPFROMSHORT(i), 0));
                if (id == SC_TASKMANAGER || id == SC_CLOSE) {
                    // Accels for these entries always work in Qt, skip them.
                    // Should be in sync with QOS2Window::handleWmTranslateAccel.
                    continue;
                }
                USHORT len = SHORT1FROMMR(WinSendMsg(subMenu, MM_QUERYITEMTEXTLENGTH, MPFROMSHORT(id), 0));
                if (len++) {
                    char *text = new char[len];
                    WinSendMsg(subMenu, MM_QUERYITEMTEXT, MPFROM2SHORT(id, len), MPFROMP(text));
                    char *tab = strrchr(text, '\t');
                    if (tab) {
                        *tab = 0;
                        WinSendMsg(subMenu, MM_SETITEMTEXT, MPFROMSHORT(id), MPFROMP(text));
                    }
                    delete[] text;
                }
            }

            // Sublclass the system menu to leave the menu mode completely when the user presses
            // the ESC key. by default, pressing ESC while the pull-down menu is showing brings us
            // to the menu bar, which is confusing in the case of the system menu, because there is
            // only one item on the menu bar, and we cannot see that it is active when the frame
            // window has an icon.
            PFNWP oldProc = WinSubclassWindow(sysMenu, QtSysMenuProc);

            // Set QtOldSysMenuProc only once: it must be the same for all FID_SYSMENU windows.
            if (!QtOldSysMenuProc)
                QtOldSysMenuProc = oldProc;
        }
    }
}

inline bool TestShowWithoutActivating(const QWindow *window)
{
    // QWidget-attribute Qt::WA_ShowWithoutActivating .
    const QVariant showWithoutActivating = window->property("_q_showWithoutActivating");
    return showWithoutActivating.isValid() && showWithoutActivating.toBool();
}

} // unnamed namespace

QHash<HWND, QOS2Window *> QOS2Window::sKnownWindows;
QPointer<QWindow> QOS2Window::sWindowUnderMouse;
QPointer<QWindow> QOS2Window::sTrackedWindow;
QWindow *QOS2Window::sPreviousCaptureWindow = nullptr;

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
    const bool isPopup = type == Qt::Popup || type == Qt::ToolTip || type == Qt::SplashScreen || (flags & Qt::FramelessWindowHint);
    const bool isDialog = (type == Qt::Dialog) || (type == Qt::Sheet) || (flags & Qt::MSWindowsFixedSizeDialogHint);
    const bool isTool = (type == Qt::Tool) || (type == Qt::Drawer);

    const QWindow *parent = isTopLevel ? window->transientParent() : window->parent();

    // QWindow::create is automatically called for parent but not for transientParent. Call
    // it now to always have a valid handle (for hwndOwner).
    if (parent)
        const_cast<QWindow *>(parent)->create();

    const QOS2Window *os2parent = parent ? static_cast<QOS2Window *>(parent->handle()) : nullptr;

    qCInfo(lcQpaWindows) << window << DV(isTopLevel) << type << flags << rect << DV(title) << DV(parent);

    Q_ASSERT(!parent || os2parent);

    const char *className;

    if (isPopup) {
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

    // Fix top level window flags in case only the type flags are passed (based on qwindowswindow.cpp).
    if (isTopLevel) {
        switch (type) {
        case Qt::Window:
            flags |= Qt::WindowTitleHint | Qt::WindowSystemMenuHint | Qt::WindowMinimizeButtonHint
                  |Qt::WindowMaximizeButtonHint|Qt::WindowCloseButtonHint;
            break;
        case Qt::Dialog:
        case Qt::Tool:
            flags |= Qt::WindowTitleHint | Qt::WindowSystemMenuHint | Qt::WindowCloseButtonHint;
            break;
        case Qt::SplashScreen:
            flags |= Qt::FramelessWindowHint;
        default:
            break;
        }
    }

    if (title.isEmpty() && flags & Qt::WindowTitleHint)
        title = isTopLevel ? qAppName() : window->objectName();

    QByteArray title8bit = title.toLocal8Bit();

    // For all top-level windows except popups we create a WC_FRAME as a parent
    // and owner and store it in mHwndFrame. Note that for popups mHwndFrame
    // will be equal to mHwnd (to unify control over top-level windows). Child
    // windows will have mHwndFrame set to NULLHANDLE to distinguish them.

    // Note also that Qt::FramelessWindowHint forces the popup mode as it's not
    // possible to have a fameless WC_FRAME (its minimal frame width is 1px).

    if (isTopLevel && !isPopup) {
        ULONG frameStyle = 0;
        ULONG frameFlags = 0;

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
        qCInfo(lcQpaWindows) << "Creating WC_FRAME" << Qt::hex << DV(frameStyle) << DV(frameFlags) << DV(hwndOwner);
        mHwndFrame = WinCreateWindow(HWND_DESKTOP, WC_FRAME, title8bit, frameStyle,
                                     0, 0, 0, 0, hwndOwner, HWND_TOP, 0,
                                     &frameData, nullptr);

        if (mHwndFrame == NULLHANDLE)
            qFatal("WinCreateWindow(WC_FRAME) failed with 0x%08lX", WinGetLastError(0));

        PFNWP oldProc = WinSubclassWindow(mHwndFrame, QtFrameProc);

        // Remember QtOldFrameProc only once: it's the same for all WC_FRAME windows.
        if (!QtOldFrameProc)
            QtOldFrameProc = oldProc;

        RemoveSysMenuAccels(mHwndFrame);

        // Create the client window.
        qCInfo(lcQpaWindows) << "Creating FID_CLIENT" << className << Qt::hex << DV(style);
        mHwnd = WinCreateWindow(mHwndFrame, className, title8bit, style, 0, 0, 0, 0,
                                mHwndFrame, HWND_TOP, FID_CLIENT, nullptr, nullptr);
    } else {
        Q_ASSERT(isPopup || hwndOwner);
        HWND hwndParent = isPopup ? HWND_DESKTOP : hwndOwner;
        qCInfo(lcQpaWindows) << "Creating child" << className << Qt::hex << DV(hwndParent) << DV(style) << DV(hwndOwner);
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

    qCInfo(lcQpaWindows) << Qt::hex << DV(mHwndFrame) << DV(mHwnd);

    // Associate mHwnd with this instance.
    sKnownWindows.insert(mHwnd, this);

    if (mHwndFrame && mHwndFrame != mHwnd) {
        // Associate mFrameHwnd with this instance.
        sKnownWindows.insert(mHwndFrame, this);

        // Position the frame window first time to have the client window resized and grab frame
        // margins (we don't trust SV_CXSIZEBORDER et al as the actual frame size depends on many
        // factors). Note that we use an artificial size of 300 x 200 for that which is big enough
        // to include the frame (it appears that the usual frame dimentions including the title bar
        // are QMargins(4, 27, 4, 5) and the minimum title bar width (with l/r farmes) is 168 px).
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
        WinSetWindowPos(mHwndFrame, hwndBehind, 0, 0, 300, 200, fl);

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
        // Note that if original x and y (in Qt coords) are zero, we assume it's a default
        // position (like QWindowCreationContext does) and avoid moving the frame beyond the top
        // left corner of the screen (this would hide the title bar etc.).
        if (rect.x() == 0 && rect.y() == 0) {
            // Compensate for y-coordinate flipping.
            y -= swp.y + mFrameMargins.top();
        } else {
            x -= swp.x;
            y -= swp.y;
        }
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
    qCInfo(lcQpaWindows) << this;

    if (hasMouseCapture())
        setMouseGrabEnabled(false);

    setKeyboardGrabEnabled(false);

    // Dessociate mHwnd and mHwndFrame from this instance.
    sKnownWindows.remove(mHwnd);
    if (mHwndFrame != NULLHANDLE && mHwndFrame != mHwnd)
        sKnownWindows.remove(mHwndFrame);

    // Deassociate mHwnd window from the instnace (we don't need any messages after this point).
    WinSetWindowPtr(mHwnd, WinData_QOS2Window, nullptr);

    // Only destroy top-level windows; children will be implicitly destroyed by their parents.
    if (mHwndFrame != NULLHANDLE)
        WinDestroyWindow(mHwndFrame);
}

void QOS2Window::initialize()
{
    qCInfo(lcQpaWindows) << this;

    setFlag(InInit);

    QWindow *w = window();
    setWindowState(w->windowStates());

    clearFlag(InInit);
}

void QOS2Window::setGeometry(const QRect &rect)
{
    // NOTE: See comments in #setWindowState.

    QWindow *w = window();

    const Qt::WindowStates state = w->windowStates ();

    qCInfo(lcQpaWindows) << this << DV(rect) << DV(state) << DV(isExposed()) << DV(w->isVisible());

    // Note: Don't call QPlatformWindow::setGeometry - it will be called indirectly from
    // #handleWmWindowPosChanged in response to WM_SIZE and/or WM_MOVE (or below, if updating the
    // normal geometry for a minimized window).

    LONG x = rect.x(), y = rect.y(), cx = rect.width(), cy = rect.height();

    if (mHwndFrame) {
        if (!mFrameMargins.isNull()) {
            // Account for the frame strut.
            if (qt_window_private(const_cast<QWindow *>(window()))->positionPolicy == QWindowPrivate::WindowFrameInclusive) {
                // This means it is a call from QWindow::setFramePosition() and
                // the coordinates include the frame (size is still the contents rectangle).
            } else {
                x -= mFrameMargins.left();
                y -= mFrameMargins.top();
            }
            cx += mFrameMargins.left() + mFrameMargins.right();
            cy += mFrameMargins.top() + mFrameMargins.bottom();
        }
        // Flip y coordinate.
        y = QOS2Screen::Height() - (y + cy);
    } else {
        // Flip y coordinate.
        y = window()->parent()->height() - (y + cy);
    }

    // If the window is minimized, change its normal geometry (see also #handleWmWindowPosChanged).
    if (mHwndFrame && (state & Qt::WindowMinimized) &&
        (normX != x || normY != y || normCX != cx || normCY != cy)) {
        WinSetWindowUShort(mHwndFrame, QWS_XRESTORE, x);
        WinSetWindowUShort(mHwndFrame, QWS_YRESTORE, y);
        WinSetWindowUShort(mHwndFrame, QWS_CXRESTORE, cx);
        WinSetWindowUShort(mHwndFrame, QWS_CYRESTORE, cy);

        // Update the normal geometry for future comparison.
        normX = x;
        normY = y;
        normCX = cx;
        normCY = cy;

        // If QWindow::handle is nullptr (e.g. when this call originates from our ctor), call
        // QPlatformWindow::setGeometry manually (see QWindowSystemInterface::handleGeometryChange).
        if (Q_UNLIKELY(!window()->handle()))
            QPlatformWindow::setGeometry(rect);
        QWindowSystemInterface::handleGeometryChange(window(), rect);
    } else {
        WinSetWindowPos(mainHwnd(), NULLHANDLE, x, y, cx, cy, SWP_SIZE | SWP_MOVE);
    }
}

QMargins QOS2Window::frameMargins() const
{
    return mFrameMargins;
}

void QOS2Window::setVisible(bool visible)
{
    // NOTE: See comments in #setWindowState.

    const QWindow *window = this->window();
    const Qt::WindowFlags flags = window->flags();
    const Qt::WindowType type = window->type();
    const Qt::WindowStates state = window->windowStates();

    qCInfo(lcQpaWindows) << this << DV(visible) << DV(state) << DV(isExposed()) << DV(window->isVisible());

    ULONG fl = 0;

    if (visible) {
        fl = SWP_SHOW;
        if (mHwndFrame) {
            if (!(type == Qt::Popup || type == Qt::ToolTip || type == Qt::Tool ||
                  TestShowWithoutActivating(window) || state & Qt::WindowMinimized))
                fl |= SWP_ACTIVATE;

            // Lazily create a window list entry when appropriate.
            if (type != Qt::Popup &&
                type != Qt::ToolTip &&
                type != Qt::Tool &&
                (type != Qt::Dialog || !window->transientParent()) &&
                (type != Qt::SplashScreen ||
                 (flags & Qt::WindowTitleHint))
            ) {
                if (!mSwEntry) {
                    PID pid;
                    WinQueryWindowProcess(mHwndFrame, &pid, NULL);
                    SWCNTRL swc;
                    memset(&swc, 0, sizeof(SWCNTRL));
                    swc.hwnd = mHwndFrame;
                    swc.idProcess = pid;
                    swc.uchVisibility = SWL_VISIBLE;
                    swc.fbJump = SWL_JUMPABLE;
                    WinQueryWindowText(mHwndFrame, sizeof(swc.szSwtitle), swc.szSwtitle);
                    mSwEntry = WinAddSwitchEntry(&swc);
                } else {
                    SWCNTRL swc;
                    WinQuerySwitchEntry(mSwEntry, &swc);
                    swc.uchVisibility = SWL_VISIBLE;
                    WinChangeSwitchEntry(mSwEntry, &swc);
                }
            }
        }
    } else {
        fl = SWP_HIDE;
        if (hasMouseCapture())
            setMouseGrabEnabled(false);
        if (mHwndFrame) {
            fl |= SWP_DEACTIVATE;
            if (mSwEntry) {
                SWCNTRL swc;
                WinQuerySwitchEntry(mSwEntry, &swc);
                swc.uchVisibility = SWL_INVISIBLE;
                WinChangeSwitchEntry(mSwEntry, &swc);
            }
        }
    }

    WinSetWindowPos(mainHwnd(), NULLHANDLE, 0, 0, 0, 0, fl);
}

bool QOS2Window::isExposed() const
{
    // NOTE: WinIsWindowShowing(mHwndFrame) will return FALSE when in fullscreen, don't use it.
    // NOTE 2: Treat minimized windows as obscure from Qt POV (see also #handleWmWindowPosChanged).

    return WinIsWindowShowing(mHwnd) && !(window()->windowStates() & Qt::WindowMinimized);
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
    qCInfo(lcQpaWindows) << this;

    // 'Active' state handling is based in focus since it needs to work for
    // child windows as well.

    if (mHwndFrame)
        WinSetWindowPos(mHwndFrame, NULLHANDLE, 0, 0, 0, 0, SWP_ACTIVATE);

    // Do not focus when blocked by the modal window.
    if (!testFlag(BlockedByModal))
        WinSetFocus(HWND_DESKTOP, mHwnd);
}

void QOS2Window::setWindowState(Qt::WindowStates state)
{
    // Window state change logic is as follows, in a lousy attempt to match Qt docs (which are very
    // uncertain in this regard) and possibly other platforms (that may differ):
    //
    // 1. Minimized state overrides everything (but can coexist with any state).
    // 2. FullScreen overrides Maximized (but can coexist with it).
    // 3. Maximized (obviously) overrides normal state (and cancels Fullscreen when comes from PM).
    // 4. Removing Minimized restores to what's left (FullScreen, Maximized or normal).
    // 5. Removing FullScreen restores to what's left (Maximized or normal) unless there is also Minimized.
    // 6. Removing Maximized restores to normal unless there is also Minimized or FullScreen.
    // 7. Restoring using PM sysmenu always restores to normal state.
    // 8. Moving/resizing a Maximized/FullScreen window doesn't cancel these states, and vice versa
    //    (this is how PM behaves as opposed e.g. to macOS).
    // 9. Moving a Minimized window programmatically from Qt doesn't change its actual position but
    //    changes its normal geometry (for later restoring).

    QWindow *w = window();

    const Qt::WindowStates oldState = testFlag(InInit) ? Qt::WindowNoState : w->windowStates ();

    qCInfo(lcQpaWindows) << this << DV(oldState) << DV(state) << DV(isExposed()) << DV(w->isVisible());

    if (oldState == state)
        return;

    if (!mHwndFrame)
        return;

    auto stateChange = oldState ^ state;

    ULONG fl = 0;
    bool enterFS = false;
    bool leaveFS = false;

    if (stateChange & Qt::WindowMinimized) {
        if (oldState & Qt::WindowFullScreen)
            leaveFS = true;
        if (state & Qt::WindowMinimized) {
            fl = SWP_MINIMIZE;
        } else if (state & Qt::WindowFullScreen) {
            leaveFS = false;
            enterFS = true;
        } else if (state & Qt::WindowMaximized) {
            fl = SWP_MAXIMIZE;
        } else if (!leaveFS) {
            fl = SWP_RESTORE;
        }
    } else if (!(state & Qt::WindowMinimized)) {
        if (stateChange & Qt::WindowFullScreen) {
            if (state & Qt::WindowFullScreen) {
                enterFS = true;
            } else {
                leaveFS = true;
                if (state & Qt::WindowMaximized) {
                    fl |= SWP_MAXIMIZE;
                }
            }
        } else if (!(state & Qt::WindowFullScreen)) {
            if (stateChange & Qt::WindowMaximized) {
                if (state & Qt::WindowMaximized) {
                    fl = SWP_MAXIMIZE;
                } else {
                    fl = SWP_RESTORE;
                }
            }
        }
    }

    qCInfo(lcQpaWindows) << this << Qt::hex << DV(fl) << DV(enterFS) << DV(leaveFS);

    if (!fl && !enterFS && !leaveFS)
        return; // Nothing to do.

    setFlag(InSetWindowState);

    if (enterFS) {
        // Remember the normal geometry (use real window flags to detect min/max state because we
        // may get here before the window is initially shown, so Qt flags can't be trusted).
        SWP swp;
        WinQueryWindowPos(mHwndFrame, &swp);
        if (swp.fl & (SWP_MINIMIZE | SWP_MAXIMIZE)) {
            normX = WinQueryWindowUShort(mHwndFrame, QWS_XRESTORE);
            normY = WinQueryWindowUShort(mHwndFrame, QWS_YRESTORE);
            normCX = WinQueryWindowUShort(mHwndFrame, QWS_CXRESTORE);
            normCY = WinQueryWindowUShort(mHwndFrame, QWS_CYRESTORE);
        } else {
            normX = swp.x;
            normY = swp.y;
            normCX = swp.cx;
            normCY = swp.cy;
        }
        qCInfo(lcQpaWindows) << this << "save normal geo" << normX << normY << normCX << normCY;

        fl = SWP_RESTORE | SWP_MOVE | SWP_SIZE; // SWP_RESTORE to cancel a possible min/max in PM
        QRect r = QOS2Screen::Instance()->geometry().marginsAdded(mFrameMargins);
        RECTL rcl = QOS2::ToRECTL(r, QOS2Screen::Height());
        WinSetWindowPos(mHwndFrame, NULLHANDLE, rcl.xLeft, rcl.yBottom,
                        rcl.xRight - rcl.xLeft, rcl.yTop - rcl.yBottom, fl);
    } else {
        if (leaveFS)
            qCInfo(lcQpaWindows) << this << "restore normal geo" << normX << normY << normCX << normCY;

        if (leaveFS && !fl) {
            fl = SWP_RESTORE | SWP_MOVE | SWP_SIZE; // SWP_RESTORE to cancel a possible min/max in PM
            WinSetWindowPos(mHwndFrame, NULLHANDLE, normX, normY, normCX, normCY, fl);
        } else {
            if (!leaveFS && fl & (SWP_MAXIMIZE | SWP_MINIMIZE) ) {
                // Remember the normal geometry when going to min/max (will be used e.g. when
                // moving/sizing a minimized window).
                normX = WinQueryWindowUShort(mHwndFrame, QWS_XRESTORE);
                normY = WinQueryWindowUShort(mHwndFrame, QWS_YRESTORE);
                normCX = WinQueryWindowUShort(mHwndFrame, QWS_CXRESTORE);
                normCY = WinQueryWindowUShort(mHwndFrame, QWS_CYRESTORE);
            }
            WinSetWindowPos(mHwndFrame, 0, 0, 0, 0, 0, fl);
            if (leaveFS && fl & (SWP_MAXIMIZE | SWP_MINIMIZE) ) {
                // Fix the normal geo in PM after min/max (which'd put fullscreen geo there).
                WinSetWindowUShort(mHwndFrame, QWS_XRESTORE, normX);
                WinSetWindowUShort(mHwndFrame, QWS_YRESTORE, normY);
                WinSetWindowUShort(mHwndFrame, QWS_CXRESTORE, normCX);
                WinSetWindowUShort(mHwndFrame, QWS_CYRESTORE, normCY);
            }
        }
    }

    clearFlag(InSetWindowState);
}

void QOS2Window::setWindowFlags(Qt::WindowFlags flags)
{
    qCInfo(lcQpaWindows) << this << DV(flags);
}

void QOS2Window::setWindowTitle(const QString &title)
{
    qCInfo(lcQpaWindows) << this << DV(title);

    if (mHwndFrame) {
        QByteArray title8bit = title.toLocal8Bit();
        WinSetWindowText(mHwndFrame, title8bit);

        if (mSwEntry) {
            SWCNTRL swc;
            WinQuerySwitchEntry(mSwEntry, &swc);
            strncpy(swc.szSwtitle, title8bit, sizeof(swc.szSwtitle)-1);
            swc.szSwtitle[sizeof(swc.szSwtitle)-1] = 0;
            WinChangeSwitchEntry(mSwEntry, &swc);
        }
    }
}

void QOS2Window::raise()
{
    qCInfo(lcQpaWindows) << this;

    if (window()->type() == Qt::Popup || !(window()->flags() & Qt::WindowStaysOnBottomHint))
        WinSetWindowPos(mainHwnd(), HWND_TOP, 0, 0, 0, 0, SWP_ZORDER);
}

void QOS2Window::lower()
{
    qCInfo(lcQpaWindows) << this;

    if (!(window()->flags() & Qt::WindowStaysOnTopHint))
        WinSetWindowPos(mainHwnd(), HWND_BOTTOM, 0, 0, 0, 0, SWP_ZORDER);
}

bool QOS2Window::setKeyboardGrabEnabled(bool grab)
{
    qCInfo(lcQpaWindows) << this << grab;

    QOS2KeyMapper *keyMapper = QOS2Integration::instance()->keyMapper();

    if (grab) {
        keyMapper->setKeyGrabber(window());
    } else {
        if (keyMapper->keyGrabber() == window())
            keyMapper->setKeyGrabber(nullptr);
    }

    return true;
}

bool QOS2Window::setMouseGrabEnabled(bool grab)
{
    qCInfo(lcQpaWindows) << this << grab;

    if (!WinIsWindowVisible(mainHwnd ()) && grab) {
        qWarning("%s: Not setting mouse grab for invisible window %s/'%s'",
                 __FUNCTION__, window()->metaObject()->className(),
                 qPrintable(window()->objectName()));
        return false;
    }

    // Override autocapture on explicit grab or on grab release.
    clearFlag(AutoMouseCapture);

    if (hasMouseCapture() == grab)
        return true;

    return WinSetCapture(HWND_DESKTOP, grab ? mHwnd : NULLHANDLE);
}

bool QOS2Window::windowEvent(QEvent *event)
{
    qCInfo(lcQpaWindows) << this << event;

    switch (event->type()) {
    case QEvent::WindowBlocked: // Blocked by another modal window.
        WinEnableWindow(mainHwnd(), FALSE);
        setFlag(BlockedByModal);
        if (hasMouseCapture())
            setMouseGrabEnabled(false);
        break;
    case QEvent::WindowUnblocked:
        clearFlag(BlockedByModal);
        WinEnableWindow(mainHwnd(), TRUE);
        break;
    default:
        break;
    }

    return QPlatformWindow::windowEvent(event);
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

void QOS2Window::handleWmClose()
{
    qCInfo(lcQpaEvents) << this;

    QWindowSystemInterface::handleCloseEvent(window());
}

void QOS2Window::handleWmActivate(MPARAM mp1)
{
    qCInfo(lcQpaEvents) << this << LONGFROMMP(mp1);

    if (LONGFROMMP(mp1))
        setFlag(Active);
    else
        clearFlag(Active);
}

void QOS2Window::handleWmSetFocus(MPARAM mp1, MPARAM mp2)
{
    HWND hwnd = (HWND)mp1;
    bool gotFocus = SHORT1FROMMP(mp2);

    QWindow *nextActiveWindow = nullptr;
    if (gotFocus) {
        nextActiveWindow = window();
    } else {
        QOS2Window *next = PlatformWindow(hwnd);
        if (next)
            nextActiveWindow = next->window();
    }

    qCInfo(lcQpaEvents) << this << Qt::hex << DV(hwnd) << DV(gotFocus) << DV(nextActiveWindow);

    QWindowSystemInterface::handleWindowActivated(nextActiveWindow);
}

bool QOS2Window::handleWmPaint()
{
    // Ignore WM_PAINT during minimized, see #handleWmWindowPosChanged.
    if (window()->windowState() & Qt::WindowMinimized)
        return false;

    // NOTE. Below we don't use WinBeginPaint because it returns a HPS with a pre-set clip region
    // which includes only paint requests coming from WinInvalidateRegion. Since Qt implements its
    // own paint request mechanism, such paint requests don't end in this clip region and would
    // therefore be clipped out leading to outdated content. We could include them by calling
    // WinInvalidateRegion in a QEvent::UpdateRequest handler or even now passing it the current Qt
    // update region for this window available through qt_dirtyRegion (as we used to do in Qt4)
    // but this looks like an overkill taking a possible region complexity into account and the
    // need to flip the y coordinate. Given that QOS2BackingStore will blit only what needs to be
    // updated from the Qt point of view, it's simpler (and faster) to just get an unclipped HPS
    // and let it go (after adding the WM_PAINT update rect to the Qt update region via expose).

    RECTL rcl = { 0, 0, 0, 0 };
    WinQueryUpdateRect(mHwnd, &rcl);

    mHps = WinGetPS(mHwnd);

    QRect updateRect = QOS2::ToQRect(rcl, geometry().height());

    qCDebug(lcQpaEvents) << this << updateRect << DV(isExposed()) << DV(window()->isVisible());

    QWindowSystemInterface::handleExposeEvent(window(), QRegion(updateRect));

    WinReleasePS(mHps);
    mHps = NULLHANDLE;

    // Validate the window to confirm that we served this WM_PAINT request (otherwise it'll keep
    // coming). Using nullptr as PRECTL is undocumented but works assuming the whole window area.
    WinValidateRect(mHwnd, nullptr, FALSE);

    return true;
}

void QOS2Window::handleWmAdjustWindowPos(MPARAM mp1)
{
    PSWP pswp =(PSWP)mp1;

    qCDebug(lcQpaEvents) << this << Qt::hex << "fl" << pswp->fl << "hwnd" << pswp->hwnd << "behind" << pswp->hwndInsertBehind;

    if (testFlag(BlockedByModal)) {
        // Forward activation requests for blocked windows to the current modal window forbid
        // placing them above it in Z-order.
        QWindow *modal = QGuiApplication::modalWindow();
        QOS2Window *os2modal = static_cast<QOS2Window *>(modal->handle());
        qCDebug(lcQpaEvents) << DV(modal) << DV(os2modal);
        if (pswp->fl & SWP_ACTIVATE) {
            pswp->fl &= ~SWP_ACTIVATE;
            WinSetActiveWindow(HWND_DESKTOP, os2modal->mainHwnd());
        }
        if (pswp->fl & SWP_ZORDER) {
            if (WinQueryWindow(pswp->hwndInsertBehind, QW_OWNER) == pswp->hwnd) {
                // It's OK to let the window be placed behind its ownee. PM needs this to maintain
                // the proper owner-ownee z-order hierarchy.
            } else {
                // Otherwise, make sure the window is behind the current modal one.
                pswp->hwndInsertBehind = os2modal->mainHwnd();
            }
        }
    }
}

void QOS2Window::handleWmWindowPosChanged(MPARAM mp1)
{
    PSWP pswp =(PSWP)mp1;

    // Reflect the exposed state when the window is shown or hidden. Note that we cannot
    // rely on WM_PAINT for the show part (as we do for unminimize in #handleWmMinMaxFrame)
    // since WM_PAINT may be deferred and Qt expects exposed changes to be instant in some
    // cases (tst_qwindow.cpp covers that).

    if (pswp->fl & SWP_SHOW) {
        // Inform that the window is obscure from the Qt POV (see also #handleWmMinMaxFrame).
        // Note that there's no need to do the opposite on un-minimize since PM will send a
        // WM_PAINT in such a case which will cause a proper expose event.
       QWindowSystemInterface::handleExposeEvent(window(), QRect(QPoint(0, 0), geometry().size()));
    } else if (pswp->fl & SWP_HIDE) {
        // Inform that the window is obscure from the Qt POV (see also #handleWmMinMaxFrame).
        // Note that there's no need to do the opposite on un-minimize since PM will send a
        // WM_PAINT in such a case which will cause a proper expose event.
       QWindowSystemInterface::handleExposeEvent(window(), QRegion());
    }

    // NOTE: When the window is minimized, PM resizes it to 40x50 and then positions it depending
    // on WPS settings for dealing with minimized windows, e.g. puts it completely off-screen to
    // a point -32000,-32000. It still then issues WM_PAINT requests for such a window which,
    // together with weird size and position, will confuse Qt in many cases since it expects
    // minimized windows to not change their size/position and not redraw themselves bt but be
    // obscure instead. Emulate this behavior by ignoring move/reisize events for them.

    if (!(pswp->fl & (SWP_SIZE | SWP_MOVE)) || (pswp->fl & SWP_MINIMIZE))
        return;

    QRect newGeo;

    // Don't use PSWP coordinates, they may belong to the frame window (i.e. include the frame).
    RECTL rcl = { 0, 0, 0, 0 };
    WinQueryWindowRect(mHwnd, &rcl);

    if (rcl.xRight > rcl.xLeft && rcl.yTop > rcl.yBottom) {
        // Get screen coordinates.
        WinMapWindowPoints(mHwnd, HWND_DESKTOP, (PPOINTL)&rcl, 2);
        newGeo = QOS2::ToQRect(rcl, QOS2Screen::Height());
    }

    QRect oldGeo = geometry();
    qCDebug(lcQpaEvents) << this << Qt::hex << pswp->fl << Qt::dec << DV(oldGeo) << DV(newGeo)
                         << DV(isExposed()) << DV(window()->isVisible());

    if (oldGeo != newGeo) {
        // If QWindow::handle is nullptr (e.g. when this call originates from our ctor), call
        // QPlatformWindow::setGeometry manually (see QWindowSystemInterface::handleGeometryChange).
        if (Q_UNLIKELY(!window()->handle()))
            QPlatformWindow::setGeometry(newGeo);
        QWindowSystemInterface::handleGeometryChange(window(), newGeo);
    }
}

void QOS2Window::handleWmMinMaxFrame(MPARAM mp1)
{
    // NOTE: See comments in #setWindowState. Note that PM is smart enough to restore from
    // Minimized to Maximized if it was so when minimizing. It seems to track this transition and
    // uses a special value of SWP::fl = 0x1880 in WM_ADJUSTWINDOWPOS (i.e. with SWP_MAXIMIZE and
    // SWP_RESTORE both set) to distinguish from a regular SWP_RESTORE. This won't work for Qt's
    // FullScreen since PM knows nowthing about it and will always act as if SWP_RESTORE is given.
    // If we ever want support for it, we should (carefully) move this code to WM_ADJUSTWINDOWPOS.
    // But it kind of makes sense to cancel FullScreen with Minimized when it comes from PM.

    if (!mHwndFrame)
        return;

    PSWP pswp = (PSWP)mp1;

    QWindow *w = window();

    const Qt::WindowStates oldState = w->windowStates();

    qCInfo(lcQpaEvents) << this << Qt::hex << pswp->fl << DV(oldState) << testFlag(InSetWindowState);

    if (!testFlag(InSetWindowState)) {
        if (pswp->fl & (SWP_MINIMIZE | SWP_MAXIMIZE)) {
            if (oldState & Qt::WindowFullScreen) {
                // Make SWP_RESTORE go to the normal geo instead of fullscreen.
                qCInfo(lcQpaEvents) << this << "restore normal geo" << normX << normY << normCX << normCY;
                WinSetWindowUShort(mHwndFrame, QWS_XRESTORE, normX);
                WinSetWindowUShort(mHwndFrame, QWS_YRESTORE, normY);
                WinSetWindowUShort(mHwndFrame, QWS_CXRESTORE, normCX);
                WinSetWindowUShort(mHwndFrame, QWS_CYRESTORE, normCY);
            }
            if (pswp->fl & SWP_MINIMIZE)
                QWindowSystemInterface::handleWindowStateChanged(w, Qt::WindowMinimized);
            else
                QWindowSystemInterface::handleWindowStateChanged(w, Qt::WindowMaximized);
        } else if (pswp->fl & SWP_RESTORE) {
            QWindowSystemInterface::handleWindowStateChanged(w, Qt::WindowNoState);
        }
    }

    if (pswp->fl & SWP_MINIMIZE) {
        // Inform that the window is obscure from the Qt POV (see also #handleWmWindowPosChanged).
        // Note that there's no need to do the opposite on un-minimize since PM will send a
        // WM_PAINT in such a case which will cause a proper expose event.
        QWindowSystemInterface::handleExposeEvent(w, QRegion());
    }
}

void QOS2Window::handleMouse(ULONG msg, MPARAM mp1, MPARAM mp2)
{
    QWindow *window = this->window();

    if (msg == WM_MOUSELEAVE) {
        qCInfo(lcQpaEvents) << "WM_MOUSELEAVE for" << window
                            << "previous window under mouse" << sWindowUnderMouse
                            << "tracked window" << sTrackedWindow;

        // When moving out of a window, WM_MOUSEMOVE within the moved-to window is received first,
        // so if TrackedWindow is not the window here, it means the cursor has left the
        // application.
        if (window == sTrackedWindow) {
            QWindow *leaveTarget = sWindowUnderMouse ? sWindowUnderMouse : sTrackedWindow;
            qCInfo(lcQpaEvents) << "Generating leave for" << leaveTarget;
            QWindowSystemInterface::handleLeaveEvent(leaveTarget);
            sTrackedWindow = nullptr;
            sWindowUnderMouse = nullptr;
        }

        return;
    }

    // Get window coordinates.
    POINTL ptl = { SHORT1FROMMP(mp1), SHORT2FROMMP(mp1) };
    const QPoint localPos = QOS2::ToQPoint(ptl, geometry().height());

    // Get screen coordinates.
    WinMapWindowPoints(mHwnd, HWND_DESKTOP, &ptl, 1);
    const QPoint globalPos = QOS2::ToQPoint(ptl, QOS2Screen::Height());

    Qt::MouseButton button;
    QEvent::Type type;

    bool discardEvent = false;

    // Get button and message type.
    switch (msg) {
    case WM_BUTTON1DOWN:
    case WM_BUTTON1DBLCLK:
        button = Qt::LeftButton;
        type = QEvent::MouseButtonPress;
        break;
    case WM_BUTTON2DOWN:
    case WM_BUTTON2DBLCLK:
        button = Qt::RightButton;
        type = QEvent::MouseButtonPress;
        break;
    case WM_BUTTON3DOWN:
    case WM_BUTTON3DBLCLK:
        button = Qt::MiddleButton;
        type = QEvent::MouseButtonPress;
        break;
    case WM_BUTTON1UP:
        button = Qt::LeftButton;
        type = QEvent::MouseButtonRelease;
        break;
    case WM_BUTTON2UP:
        button = Qt::RightButton;
        type = QEvent::MouseButtonRelease;
        break;
    case WM_BUTTON3UP:
        button = Qt::MiddleButton;
        type = QEvent::MouseButtonRelease;
        break;
    case WM_MOUSEMOVE:
        button = Qt::NoButton;
        type = QEvent::MouseMove;
        break;
    default:
        Q_ASSERT (false);
        button = Qt::NoButton;
        type = QEvent::None;
    }

    Qt::MouseButtons buttons;

    // Get other button state.
    if (WinGetKeyState(HWND_DESKTOP, VK_BUTTON1) & 0x8000)
        buttons |= Qt::LeftButton;
    if (WinGetKeyState(HWND_DESKTOP, VK_BUTTON2) & 0x8000)
        buttons |= Qt::RightButton;
    if (WinGetKeyState(HWND_DESKTOP, VK_BUTTON3) & 0x8000)
        buttons |= Qt::MiddleButton;

    Qt::KeyboardModifiers modifiers;
    const USHORT flags = SHORT2FROMMP(mp2);
    const int extraKeyState = QOS2Integration::instance()->keyMapper()->extraKeyState();

    // Get key modifiers.
    if (flags & KC_SHIFT)
        modifiers |= Qt::ShiftModifier;
    if (flags & KC_CTRL)
        modifiers |= Qt::ControlModifier;
    if ((flags & KC_ALT) || (extraKeyState & Qt::AltModifier))
        modifiers |= Qt::AltModifier;
    if (extraKeyState & Qt::MetaModifier)
        modifiers |= Qt::MetaModifier;

    // MouseMove are REALLY annoying and not worth it (there is lcQpaMessages for low-level PM flow).
    if (type != QEvent::MouseMove)
        qCDebug(lcQpaEvents) << this << Qt::hex << DV (msg) << DV (flags) << Qt::dec << DV (ptl.x) << DV (ptl.y)
                             << DV (globalPos) << DV (localPos)
                             << DV (button) << DV (buttons) << DV (type) << DV (modifiers);

    // Qt expects the platform plugin to capture the mouse on
    // any button press until release.
    if (type == QEvent::MouseButtonPress && !hasMouseCapture()) {
        qCInfo(lcQpaEvents) << this << "Setting automatic mouse capture for" << window;
        setMouseGrabEnabled(true);
        setFlag(AutoMouseCapture); // Important: after #setMouseGrabEnabled.
    } else if (type == QEvent::MouseButtonRelease && hasMouseCapture() && testFlag(AutoMouseCapture)) {
        qCInfo(lcQpaEvents) << this << "Releasing automatic mouse capture for" << window;
        setMouseGrabEnabled(false);
    }

    QWindow *currentWindowUnderMouse = hasMouseCapture() ? QOS2::WindowAt(globalPos) : window;
    while (currentWindowUnderMouse && currentWindowUnderMouse->flags() & Qt::WindowTransparentForInput)
        currentWindowUnderMouse = currentWindowUnderMouse->parent();
    if (!currentWindowUnderMouse) {
        const QRect clientRect(QPoint(0, 0), window->size());
        if (clientRect.contains(localPos))
            currentWindowUnderMouse = window;
    }

    const bool hasCapture = hasMouseCapture();
    const bool currentNotCapturing = hasCapture && currentWindowUnderMouse != window;

    // Enter new window: track to generate leave event.
    // If there is an active capture, only track if the current window is capturing,
    // so we don't get extra leave when cursor leaves the application.
    if (window != sTrackedWindow && !currentNotCapturing) {
        sTrackedWindow = window;
    }

    // No enter or leave events are sent as long as there is an autocapturing window.
    if (!hasCapture || !testFlag(AutoMouseCapture)) {
        // Leave is needed if:
        // 1) There is no capture and we move from a window to another window.
        //    Note: Leaving the application entirely is handled in WM_MOUSELEAVE case.
        // 2) There is capture and we move out of the capturing window.
        // 3) There is a new capture and we were over another window.
        if ((sWindowUnderMouse && sWindowUnderMouse != currentWindowUnderMouse
                && (!hasCapture || window == sWindowUnderMouse))
            || (hasCapture && sPreviousCaptureWindow != window && sWindowUnderMouse
                && sWindowUnderMouse != window)) {
            qCInfo(lcQpaEvents) << "Generating synthetic leave for" << sWindowUnderMouse;
            QWindowSystemInterface::handleLeaveEvent(sWindowUnderMouse);
            if (type == QEvent::MouseMove) {
                // PM may also send a dummy WM_MOUSEMOVE event for enter/leave events, ignore it
                // for consistency with X11 and macOS (tst_QWindow checks that).
                discardEvent = true;
            }
            if (currentNotCapturing) {
                // Clear tracking if capturing and current window is not the capturing window
                // to avoid leave when mouse actually leaves the application.
                sTrackedWindow = nullptr;
            }
        }
        // Enter is needed if:
        // 1) There is no capture and we move to a new window.
        // 2) There is capture and we move into the capturing window.
        // 3) The capture just ended and we are over non-capturing window.
        if ((currentWindowUnderMouse && sWindowUnderMouse != currentWindowUnderMouse
                && (!hasCapture || currentWindowUnderMouse == window))
            || (sPreviousCaptureWindow && window != sPreviousCaptureWindow && currentWindowUnderMouse
                && currentWindowUnderMouse != sPreviousCaptureWindow)) {
            QPoint localPos;
            qCInfo(lcQpaEvents) << "Generating enter for" << currentWindowUnderMouse;
            if (QOS2Window *platformWindow = static_cast<QOS2Window *>(currentWindowUnderMouse->handle())) {
                localPos = platformWindow->mapFromGlobal(globalPos);
            }
            QWindowSystemInterface::handleEnterEvent(currentWindowUnderMouse, localPos, globalPos);
            if (type == QEvent::MouseMove) {
                // PM may also send a dummy WM_MOUSEMOVE event for enter/leave events, ignore it
                // for consistency with X11 and macOS (tst_QWindow checks that).
                discardEvent = true;
            }
        }
        // We need to track WindowUnderMouse separately from TrackedWindow, as
        // PM will not trigger WM_MOUSELEAVE for leaving window when mouse capture is set.
        sWindowUnderMouse = currentWindowUnderMouse;
    }

    if (!discardEvent)
        QWindowSystemInterface::handleMouseEvent(window, localPos, globalPos,
                                                 buttons, button, type, modifiers, Qt::MouseEventNotSynthesized);

    sPreviousCaptureWindow = hasCapture ? window : nullptr;
}

void QOS2Window::handleWheel(ULONG msg, MPARAM mp1, MPARAM mp2)
{
    enum { WHEEL_DELTA = 120 };

    POINTL ptl;
    WinQueryMsgPos(NULLHANDLE, &ptl);

    // Consume duplicate wheel events sent by the AMouse driver to emulate multiline scrolls. We
    // need this since currently Qt (QScrollBar, for instance) maintains the number of lines to
    // scroll per wheel rotation (including the special handling of CTRL and SHIFT modifiers) on
    // its own and doesn't have a setting to tell it to be aware of system settings for the mouse
    // wheel. If we had processed events as they are, we would get a confusing behavior (too many
    // lines scrolled etc.).
    {
        QMSG wheelMsg;
        while (WinPeekMsg(0, &wheelMsg, mHwnd, msg, msg, PM_NOREMOVE)) {
            // PM BUG: ptl contains SHORT coordinates although fields are LONG.
            wheelMsg.ptl.x = (short) wheelMsg.ptl.x;
            wheelMsg.ptl.y = (short) wheelMsg.ptl.y;
            if (wheelMsg.mp1 != mp1 ||
                wheelMsg.mp2 != mp2 ||
                wheelMsg.ptl.x != ptl.x ||
                wheelMsg.ptl.y != ptl.y)
                break;
            WinPeekMsg(NULLHANDLE, &wheelMsg, mHwnd, msg, msg, PM_REMOVE);
        }
    }

    // Get screen coordinates.
    const QPoint globalPos = QOS2::ToQPoint(ptl, QOS2Screen::Height());

    // Get window coordinates.
    WinMapWindowPoints(HWND_DESKTOP, mHwnd, &ptl, 1);
    const QPoint localPos = QOS2::ToQPoint(ptl, geometry().height());

    int delta;

    // Get wheel delta.
    switch (SHORT2FROMMP(mp2)) {
        case SB_LINEUP:
        case SB_PAGEUP:
            delta = WHEEL_DELTA;
            break;
        case SB_LINEDOWN:
        case SB_PAGEDOWN:
            delta = -WHEEL_DELTA;
            break;
        default:
            return;
    }

    Qt::KeyboardModifiers modifiers;
    const int extraKeyState = QOS2Integration::instance()->keyMapper ()->extraKeyState ();

    // Get key modifiers.
    if (WinGetKeyState(HWND_DESKTOP, VK_SHIFT ) & 0x8000)
        modifiers |= Qt::ShiftModifier;
    if ((WinGetKeyState(HWND_DESKTOP, VK_ALT) & 0x8000) || (extraKeyState & Qt::AltModifier))
        modifiers |= Qt::AltModifier;
    if (WinGetKeyState(HWND_DESKTOP, VK_CTRL) & 0x8000)
        modifiers |= Qt::ControlModifier;
    if (extraKeyState & Qt::MetaModifier)
        modifiers |= Qt::MetaModifier;

    // Alt inverts scroll orientation (Qt/Win32 behavior)
    const QPoint point = (msg == WM_VSCROLL || modifiers & Qt::AltModifier) ? QPoint(0, delta) : QPoint(delta, 0);

    qCDebug(lcQpaEvents) << this << Qt::hex << DV (msg) << DV (mp2) << Qt::dec << DV (ptl.x) << DV (ptl.y)
                         << DV (globalPos) << DV (localPos) << DV (point) << DV (modifiers);

    QWindowSystemInterface::handleWheelEvent(window(), localPos, globalPos, QPoint(), point, modifiers);
}

bool QOS2Window::handleWmChar(MPARAM mp1, MPARAM mp2)
{
    CHRMSG chm;

    ((PMPARAM)&chm)[0] = mp1;
    ((PMPARAM)&chm)[1] = mp2;

    qCDebug(lcQpaEvents) << this << Qt::hex << DV(chm.fs) << DV(chm.scancode) << DV(chm.vkey) << DV(chm.chr) << Qt::dec << DV(chm.cRepeat);

    return QOS2Integration::instance()->keyMapper()->translateKeyEvent(this, mHwnd, chm);
}

bool QOS2Window::handleWmTranslateAccel(MPARAM mp1, MPARAM mp2)
{
    if (mHwndFrame) {
        MRESULT mrc = WinDefWindowProc(mHwnd, WM_TRANSLATEACCEL, mp1, mp2);
        if (mrc) {
            QMSG &qmsg = *(QMSG*)mp1;
            if (qmsg.msg == WM_SYSCOMMAND && WinWindowFromID(mHwndFrame, FID_SYSMENU)) {
                switch (SHORT1FROMMP(qmsg.mp1)) {
                    // Should be in sync with RemoveSysMenuAccels.
                    case SC_CLOSE:
                    case SC_TASKMANAGER:
                        return (MRESULT)TRUE;
                    default:
                        break;
                }
            }
        }
    }

    // Return FALSE in all other cases to let Qt process keystrokes that are in the system-wide
    // frame accelerator table.
    return false;
}

#ifndef QT_NO_DEBUG_STREAM
QDebug operator<<(QDebug d, const QOS2Window *window)
{
    QDebugStateSaver saver(d);
    d.nospace();
    d <<  "QOS2Window(" << (void *)window;
    if (window)
        d << ',' << Qt::hex << window->hwndFrame() << ',' << window->hwnd() << ',' << Qt::dec << window->geometry()
          << ',' << window->window();
    d << ')';
    return d;
}
#endif // QT_NO_DEBUG_STREAM

QT_END_NAMESPACE
