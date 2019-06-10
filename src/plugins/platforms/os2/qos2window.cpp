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
    QOS2Window *that = static_cast<QOS2Window *>(WinQueryWindowPtr(hwnd, WinData_QOS2Window));

    // WinData_QOS2Window is null during widnow creation, ignore this case for now.
    if (that) {
        Q_ASSERT(that->hwnd() == hwnd);
        if (msg >= WM_MOUSEFIRST && msg <= WM_MOUSELAST) {
            that->handleMouse(msg, mp1, mp2);
            // NOTE: pass mouse events to WinDefWindowProc to cause window activation on click etc.
        } else {
            switch (msg) {
            case WM_CLOSE: that->handleWmClose(); return 0;
            case WM_ACTIVATE: that->handleWmActivate(mp1); return 0;
            case WM_PAINT: that->handleWmPaint(); return 0;
            case WM_WINDOWPOSCHANGED: {
                // Handle size/move changes if not already done in QtFrameProc.
                if (!that->hwndFrame() || that->hwndFrame() == that->hwnd())
                    that->handleSizeMove();
                break;
            }
            case WM_VSCROLL:
            case WM_HSCROLL:
                that->handleWheel(msg, mp1, mp2); return 0;
            case WM_CHAR:
                if (that->handleWmChar(mp1, mp2)) return (MRESULT)TRUE;
                break;
            default: break;
            }
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
        qCInfo(lcQpaWindows) << "Creating WC_FRAME" << hex << DV(frameStyle) << DV(frameFlags) << DV(hwndOwner);
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
        qCInfo(lcQpaWindows) << "Creating FID_CLIENT" << className << hex << DV(style);
        mHwnd = WinCreateWindow(mHwndFrame, className, title8bit, style, 0, 0, 0, 0,
                                mHwndFrame, HWND_TOP, FID_CLIENT, nullptr, nullptr);
    } else {
        Q_ASSERT(isPopup || hwndOwner);
        HWND hwndParent = isPopup ? HWND_DESKTOP : hwndOwner;
        qCInfo(lcQpaWindows) << "Creating child" << className << hex << DV(hwndParent) << DV(style) << DV(hwndOwner);
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
    qCInfo(lcQpaWindows) << this;

    if (hasMouseCapture())
        setMouseGrabEnabled(false);

    // Deassociate mHwnd window from the instnace (we don't need any messages after this point).
    WinSetWindowPtr(mHwnd, WinData_QOS2Window, nullptr);

    // Only destroy top-level windows; children will be implicitly destroyed by their parents.
    if (mHwndFrame != NULLHANDLE)
        WinDestroyWindow(mHwndFrame);
}

void QOS2Window::setGeometry(const QRect &rect)
{
    qCInfo(lcQpaWindows) << this << DV(rect);

    // Note: Don't call QPlatformWindow::setGeometry - it will be called indirectly from
    // #handleSizeMove in response to WM_SIZE and/or WM_MOVE.

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

    WinSetWindowPos(mHwndFrame, NULLHANDLE, x, y, cx, cy, SWP_SIZE | SWP_MOVE);
}

QMargins QOS2Window::frameMargins() const
{
    return mFrameMargins;
}

void QOS2Window::setVisible(bool visible)
{
    qCInfo(lcQpaWindows) << this << DV(visible);

    if (visible) {
        if (mHwndFrame) {
            // Lazily create a window list entry when appropriate.
            Qt::WindowType type = window()->type();
            if (type != Qt::Popup &&
                type != Qt::ToolTip &&
                type != Qt::Tool &&
                (type != Qt::Dialog || !window()->transientParent()) &&
                (type != Qt::SplashScreen ||
                 (window()->flags() & Qt::WindowTitleHint))
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
        if (hasMouseCapture())
            setMouseGrabEnabled(false);
        if (mHwndFrame) {
            if (mSwEntry) {
                SWCNTRL swc;
                WinQuerySwitchEntry(mSwEntry, &swc);
                swc.uchVisibility = SWL_INVISIBLE;
                WinChangeSwitchEntry(mSwEntry, &swc);
            }
        }
    }

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
    qCInfo(lcQpaWindows) << this;

    if (mHwndFrame)
        WinSetWindowPos(mHwndFrame, NULLHANDLE, 0, 0, 0, 0, SWP_ACTIVATE);
}

void QOS2Window::setWindowState(Qt::WindowStates state)
{
    qCInfo(lcQpaWindows) << this << DV(state);
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

void QOS2Window::windowEvent(QEvent *event)
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

void QOS2Window::handleWmPaint()
{
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

    RECTL rcl;
    WinQueryUpdateRect(mHwnd, &rcl);

    mHps = WinGetPS(mHwnd);

    QRect updateRect = QOS2::ToQRect(rcl, geometry().height());

    qCDebug(lcQpaEvents) << this << updateRect <<  QOS2::ToQRect(rcl, geometry().height());

    QWindowSystemInterface::handleExposeEvent(window(), QRegion(updateRect));

    WinReleasePS(mHps);
    mHps = NULLHANDLE;

    // Validate the window to confirm that we served this WM_PAINT request (otherwise it'll keep
    // coming). Using nullptr as PRECTL is undocumented but works assuming the whole window area.
    WinValidateRect(mHwnd, nullptr, FALSE);
}

void QOS2Window::handleWmAdjustWindowPos(MPARAM mp1)
{
    PSWP pswp =(PSWP)mp1;

    qCDebug(lcQpaEvents) << this << hex << "fl" << pswp->fl << "hwnd" << pswp->hwnd << "behind" << pswp->hwndInsertBehind;

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
    qCDebug(lcQpaEvents) << this << DV(oldGeo) << DV(newGeo);

    if (oldGeo != newGeo) {
        // If QWindow::handle is nullptr (e.g. when this call originates from our ctor), call
        // QPlatformWindow::setGeometry manually (see QWindowSystemInterface::handleGeometryChange).
        if (Q_UNLIKELY(!window()->handle()))
            QPlatformWindow::setGeometry(newGeo);
        QWindowSystemInterface::handleGeometryChange(window(), newGeo);
    }
}

void QOS2Window::handleMouse(ULONG msg, MPARAM mp1, MPARAM mp2)
{
    // Get window coordinates.
    POINTL ptl = { SHORT1FROMMP(mp1), SHORT2FROMMP(mp1) };
    const QPoint localPos = QOS2::ToQPoint(ptl, geometry().height());

    // Get screen coordinates.
    WinMapWindowPoints(mHwnd, HWND_DESKTOP, &ptl, 1);
    const QPoint globalPos = QOS2::ToQPoint(ptl, QOS2Screen::Height());

    Qt::MouseButton button;
    QEvent::Type type;

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
        button = Qt::MidButton;
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
        button = Qt::MidButton;
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
        buttons |= Qt::MidButton;

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

    qCDebug(lcQpaEvents) << this << hex << DV (msg) << DV (flags) << dec << DV (ptl.x) << DV (ptl.y)
                         << DV (globalPos) << DV (localPos)
                         << DV (button) << DV (buttons) << DV (type) << DV (modifiers);

    // Qt expects the platform plugin to capture the mouse on
    // any button press until release.
    if (type == QEvent::MouseButtonPress && !hasMouseCapture()) {
        qCInfo(lcQpaEvents) << this << "Setting automatic mouse capture for" << window();
        setMouseGrabEnabled(true);
        setFlag(AutoMouseCapture); // Important: after #setMouseGrabEnabled.
    } else if (type == QEvent::MouseButtonRelease && hasMouseCapture() && testFlag(AutoMouseCapture)) {
        qCInfo(lcQpaEvents) << this << "Releasing automatic mouse capture for" << window();
        setMouseGrabEnabled(false);
    }

    QWindowSystemInterface::handleMouseEvent(window(), localPos, globalPos,
                                             buttons, button, type, modifiers, Qt::MouseEventNotSynthesized);
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

    qCDebug(lcQpaEvents) << this << hex << DV (msg) << DV (mp2) << dec << DV (ptl.x) << DV (ptl.y)
                         << DV (globalPos) << DV (localPos) << DV (point) << DV (modifiers);

    QWindowSystemInterface::handleWheelEvent(window(), localPos, globalPos, QPoint(), point, modifiers);
}

bool QOS2Window::handleWmChar(MPARAM mp1, MPARAM mp2)
{
    CHRMSG chm;

    ((PMPARAM)&chm)[0] = mp1;
    ((PMPARAM)&chm)[1] = mp2;

    qCDebug(lcQpaEvents) << this << hex << DV(chm.fs) << DV(chm.scancode) << DV(chm.vkey) << DV(chm.chr) << dec << DV(chm.cRepeat);

    return QOS2Integration::instance()->keyMapper()->translateKeyEvent(this, mHwnd, chm);
}

#ifndef QT_NO_DEBUG_STREAM
QDebug operator<<(QDebug d, const QOS2Window *window)
{
    QDebugStateSaver saver(d);
    d.nospace();
    d <<  "QOS2Window(" << (void *)window;
    if (window)
        d << ',' << hex << window->hwndFrame() << ',' << window->hwnd() << ',' << dec << window->geometry()
          << ',' << window->window();
    d << ')';
    return d;
}
#endif // QT_NO_DEBUG_STREAM

QT_END_NAMESPACE
