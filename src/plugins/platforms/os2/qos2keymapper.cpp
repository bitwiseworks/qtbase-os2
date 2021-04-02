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

#include "qos2keymapper.h"
#include "qos2window.h"

#include <qpa/qwindowsysteminterface.h>

#include <QtGui/QGuiApplication>

// BIDI API --------------------------------------------------------[ start ] --
// copied from pmbidi.h from the OS/2 toolkit as Innotek GCC headers lack this

#define KL_LATIN     0x00000000
#define KL_NATIONAL  0x00000001

ULONG   APIENTRY WinSetKbdLayer     (HWND hwnd,
                                     ULONG idKbdLayer,
                                     ULONG flFlags);

ULONG   APIENTRY WinQueryKbdLayer   (HWND hwnd);

ULONG   APIENTRY WinQueryKbdLayout  (HWND hwndDesktop);

BOOL    APIENTRY WinSetKbdLayout    (HWND hwndDesktop,
                                     ULONG idKbdLayout);

// BIDI API ----------------------------------------------------------[ end ] --

QT_BEGIN_NAMESPACE

namespace {

const bool lcQpaEventsDebug = lcQpaEvents().isDebugEnabled();

// Key recorder ----------------------------------------------------[ start ] --

struct KeyRecord {
    KeyRecord(int _scan, int _code, int _state, const QString &_text)
        : scan(_scan), code(_code), state(_state), text(_text) {}
    KeyRecord() {}

    int scan;
    int code;
    int state;
    QString text;
};

constexpr const int QT_MAX_KEY_RECORDINGS = 64; // User has LOTS of fingers...

struct KeyRecorder
{
    KeyRecorder() : nrecs(0) {}

    inline KeyRecord *findKey(int scan, bool remove);
    inline void storeKey(int scan, int code, int state, const QString& text);
    inline void clearKeys();

    int nrecs;
    KeyRecord deleted_record; // A copy of last entry removed from records[]
    KeyRecord records[QT_MAX_KEY_RECORDINGS];
};
KeyRecorder KeyRecorder;

void ClearKeyRecorderOnApplicationInActive(Qt::ApplicationState state)
{
    if (state == Qt::ApplicationInactive)
        KeyRecorder.clearKeys();
}

KeyRecord *KeyRecorder::findKey(int scan, bool remove)
{
    KeyRecord *result = 0;

    if(scan == 0) // DBCS chars or user-injected keys
 	  	return result;

    for (int i = 0; i < nrecs; ++i) {
        if (records[i].scan == scan) {
            if (remove) {
                deleted_record = records[i];
                // Move rest down, and decrease count
                while (i + 1 < nrecs) {
                    records[i] = records[i + 1];
                    ++i;
                }
                --nrecs;
                result = &deleted_record;
            } else {
                result = &records[i];
            }
            break;
        }
    }
    return result;
}

void KeyRecorder::storeKey(int scan, int code, int state, const QString& text)
{
    if(scan == 0 && code == 0) // DBCS chars or user-injected keys
        return;

    Q_ASSERT_X(nrecs != QT_MAX_KEY_RECORDINGS,
               "Internal KeyRecorder",
               "Keyboard recorder buffer overflow, consider increasing QT_MAX_KEY_RECORDINGS");

    if (nrecs == QT_MAX_KEY_RECORDINGS) {
        qWarning("Qt: Internal keyboard buffer overflow");
        return;
    }
    records[nrecs++] = KeyRecord(scan, code, state, text);
}

void KeyRecorder::clearKeys()
{
    nrecs = 0;
}
// Key recorder ------------------------------------------------------[ end ] --

// Key translation -------------------------------------------------[ start ] --

// Meaning of values:
//             0 = Character output key, needs keyboard driver mapping
//   Key_unknown = Unknown Virtual Key, no translation possible, ignore
const uint KeyTbl[] = { // Keyboard mapping table
                        // Dec |  Hex | PM Virtual key
    Qt::Key_unknown,    //   0   0x00
    Qt::Key_unknown,    //   1   0x01   VK_BUTTON1          | Mouse button 1
    Qt::Key_unknown,    //   2   0x02   VK_BUTTON2          | Mouse button 2
    Qt::Key_unknown,    //   3   0x03   VK_BUTTON3          | Mouse button 3
    Qt::Key_Cancel,     //   4   0x04   VK_BREAK            | Control-Break processing
    Qt::Key_Backspace,  //   5   0x05   VK_BACKSPACE        | BackSpace key
    Qt::Key_Tab,        //   6   0x06   VK_TAB              | Tab key
    Qt::Key_Backtab,    //   7   0x07   VK_BACKTAB          | Shift+Tab key
    Qt::Key_Return,     //   8   0x08   VK_RETURN           | Enter key
    Qt::Key_Shift,      //   9   0x09   VK_SHIFT            | Shift key
    Qt::Key_Control,    //  10   0x0A   VK_CTRL             | Ctrl key
    Qt::Key_Alt,        //  11   0x0B   VK_ALT              | Alt key
    Qt::Key_Alt,        //  12   0x0C   VK_ALTGRAF          | AltGr key
    Qt::Key_Pause,      //  13   0x0D   VK_PAUSE            | Pause key
    Qt::Key_CapsLock,   //  14   0x0E   VK_CAPSLOCK         | Caps-Lock
    Qt::Key_Escape,     //  15   0x0F   VK_ESC              | Esc key
    Qt::Key_Space,      //  16   0x10   VK_SPACE            | Spacebar
    Qt::Key_PageUp,     //  17   0x11   VK_PAGEUP           | Page Up key
    Qt::Key_PageDown,   //  18   0x12   VK_PAGEDOWN         | Page Down key
    Qt::Key_End,        //  19   0x13   VK_END              | End key
    Qt::Key_Home,       //  20   0x14   VK_HOME             | Home key
    Qt::Key_Left,       //  21   0x15   VK_LEFT             | Left arrow key
    Qt::Key_Up,         //  22   0x16   VK_UP               | Up arrow key
    Qt::Key_Right,      //  23   0x17   VK_RIGHT            | Right arrow key
    Qt::Key_Down,       //  24   0x18   VK_DOWN             | Down arrow key
    Qt::Key_Print,      //  25   0x19   VK_PRINTSCRN        | Print Screen key
    Qt::Key_Insert,     //  26   0x1A   VK_INSERT           | Ins key
    Qt::Key_Delete,     //  27   0x1B   VK_DELETE           | Del key
    Qt::Key_ScrollLock, //  28   0x1C   VK_SCROLL           | Scroll Lock key
    Qt::Key_NumLock,    //  29   0x1D   VK_NUMLOCK          | Num Lock key
    Qt::Key_Enter,      //  30   0x1E   VK_ENTER            | Enter (Numpad) key
    Qt::Key_SysReq,     //  31   0x1F   VK_SYSRQ            | SysReq key
    Qt::Key_F1,         //  32   0x20   VK_F1               | F1 key
    Qt::Key_F2,         //  33   0x21   VK_F2               | F2 key
    Qt::Key_F3,         //  34   0x22   VK_F3               | F3 key
    Qt::Key_F4,         //  35   0x23   VK_F4               | F4 key
    Qt::Key_F5,         //  36   0x24   VK_F5               | F5 key
    Qt::Key_F6,         //  37   0x25   VK_F6               | F6 key
    Qt::Key_F7,         //  38   0x26   VK_F7               | F7 key
    Qt::Key_F8,         //  39   0x27   VK_F8               | F8 key
    Qt::Key_F9,         //  40   0x28   VK_F9               | F9 key
    Qt::Key_F10,        //  41   0x29   VK_F10              | F10 key
    Qt::Key_F11,        //  42   0x2A   VK_F11              | F11 key
    Qt::Key_F12,        //  43   0x2B   VK_F12              | F12 key
    Qt::Key_F13,        //  44   0x2C   VK_F13              | F13 key
    Qt::Key_F14,        //  45   0x2D   VK_F14              | F14 key
    Qt::Key_F15,        //  46   0x2E   VK_F15              | F15 key
    Qt::Key_F16,        //  47   0x2F   VK_F16              | F16 key
    Qt::Key_F17,        //  48   0x30   VK_F17              | F17 key
    Qt::Key_F18,        //  49   0x31   VK_F18              | F18 key
    Qt::Key_F19,        //  50   0x32   VK_F19              | F19 key
    Qt::Key_F20,        //  51   0x33   VK_F20              | F20 key
    Qt::Key_F21,        //  52   0x34   VK_F21              | F21 key
    Qt::Key_F22,        //  53   0x35   VK_F22              | F22 key
    Qt::Key_F23,        //  54   0x36   VK_F23              | F23 key
    Qt::Key_F24,        //  55   0x37   VK_F24              | F24 key
    Qt::Key_unknown,    //  56   0x38   VK_ENDDRAG          | ???
    Qt::Key_Clear,      //  57   0x39   VK_CLEAR            | Clear key
    Qt::Key_unknown,    //  58   0x3A   VK_EREOF            | ???
    Qt::Key_unknown,    //  59   0x3B   VK_PA1              | ???
    Qt::Key_unknown,    //  60   0x3C   VK_ATTN             | ???
    Qt::Key_unknown,    //  61   0x3D   VK_CRSEL            | ???
    Qt::Key_unknown,    //  62   0x3E   VK_EXSEL            | ???
    Qt::Key_unknown,    //  63   0x3F   VK_COPY             | ???
    Qt::Key_unknown,    //  64   0x40   VK_BLK1             | ???
    Qt::Key_unknown,    //  65   0x41   VK_BLK2             | ???
};

// Converts known accent symbols to their Key_Dead_ equivalents.
static inline int DeadCharToDeadKeyCode(const QChar &ch)
{
    switch (ch.unicode()) {
    case 0x0060: return Qt::Key_Dead_Grave;
    case 0x00B4: return Qt::Key_Dead_Acute;
    case 0x005E: return Qt::Key_Dead_Circumflex;
    case 0x007E: return Qt::Key_Dead_Tilde;
    case 0x00AF: return Qt::Key_Dead_Macron;
    case 0x02D8: return Qt::Key_Dead_Breve;
    case 0x02D9: return Qt::Key_Dead_Abovedot;
    case 0x00A8: return Qt::Key_Dead_Diaeresis;
    case 0x02DA: return Qt::Key_Dead_Abovering;
    case 0x02DD: return Qt::Key_Dead_Doubleacute;
    case 0x02C7: return Qt::Key_Dead_Caron;
    case 0x00B8: return Qt::Key_Dead_Cedilla;
    case 0x02DB: return Qt::Key_Dead_Ogonek;
    case 0x1DA5: return Qt::Key_Dead_Iota; // TODO is that correct???
    case 0x3099: return Qt::Key_Dead_Voiced_Sound;
    case 0x309A: return Qt::Key_Dead_Semivoiced_Sound;
    case 0x0323: return Qt::Key_Dead_Belowdot;
    case 0x02DE: return Qt::Key_Dead_Hook;
    case 0x031B: return Qt::Key_Dead_Horn;
    default: break;
    }

    // Otherwise, return unmodified uppercase unicode value (similar to other cases).
    return ch.unicode();
}

// Possible Qt modifier states. Must match KbdModsTbl.
const Qt::KeyboardModifiers QtModsTbl[KeyboardLayoutItem::QtKeySize] = {
    Qt::NoModifier,                                             // 0
    Qt::ShiftModifier,                                          // 1
    Qt::ControlModifier,                                        // 2
    Qt::ControlModifier | Qt::ShiftModifier,                    // 3
    Qt::AltModifier,                                            // 4
    Qt::AltModifier | Qt::ShiftModifier,                        // 5
    Qt::AltModifier | Qt::ControlModifier,                      // 6
    Qt::AltModifier | Qt::ShiftModifier | Qt::ControlModifier,  // 7
};

// Possible OS/2 keyboard modifier states. Must match QtModsTbl.
const USHORT KbdModsTbl[] = {
    0,                                                          // 0
    KBDSTF_RIGHTSHIFT,                                          // 1
    KBDSTF_CONTROL,                                             // 2
    KBDSTF_CONTROL | KBDSTF_RIGHTSHIFT,                         // 3
    KBDSTF_ALT,                                                 // 4
    KBDSTF_ALT | KBDSTF_RIGHTSHIFT,                             // 5
    KBDSTF_ALT | KBDSTF_CONTROL,                                // 6
    KBDSTF_ALT | KBDSTF_RIGHTSHIFT | KBDSTF_CONTROL,            // 7
};

// Key translation ---------------------------------------------------[ end ]---

} // namespace

QOS2KeyMapper::QOS2KeyMapper()
{
    memset(mKeyLayout, 0, sizeof(mKeyLayout));
    QGuiApplication *app = static_cast<QGuiApplication *>(QGuiApplication::instance());
    QObject::connect(app, &QGuiApplication::applicationStateChanged,
                     app, ClearKeyRecorderOnApplicationInActive);
}

bool QOS2KeyMapper::translateKeyEvent(QOS2Window *window, HWND hwnd, CHRMSG &chm)
{
    if (!(chm.fs & KC_KEYUP))
        updateKeyMap(hwnd, chm);

    bool k0 = false;
    bool k1 = false;

    QWindow *receiver = mKeyGrabber ? mKeyGrabber : window->window();

    // We combine the flags from the message with the raw chr value and pass
    // them as QKeyEvent::nativeModifiers(). Together with chr.vkey passed as
    // nativeVirtualKey() and chr.scancode as nativeScanCode(), the Qt
    // application gets the full key message details (except the repeat count).
    quint32 nativeMods = chm.fs | chm.chr << 16;

    // Get the modifier states (may be altered later, depending on key code)
    int state = 0;
    if (chm.fs & KC_SHIFT)
        state |= Qt::ShiftModifier;
    if (chm.fs & KC_CTRL)
        state |= Qt::ControlModifier;
    if (chm.fs & KC_ALT)
        state |= Qt::AltModifier;

    // Translate VK_* (native) -> Key_* (Qt) keys
    int code = 0;
    if ((chm.fs & KC_VIRTUALKEY)) {
        if (chm.vkey == 0) {
            // The only known situation when KC_VIRTUALKEY is present but
            // vkey is zero is when Alt+Shift is pressed to switch the
            // keyboard layout state from latin to national and back.
            // It seems that this way the system informs applications about
            // layout changes: chm.chr is 0xF1 when the user switches
            // to the national layout (i.e. presses Alt + Left Shift)
            // and 0xF0 when he switches back (presses Alt + Right Shift).
            // We assume this and restore fs, vkey, scancode and chr accordingly.
            if (chm.chr == 0xF0 || chm.chr == 0xF1) {
                chm.fs |= KC_ALT | KC_SHIFT;
                chm.vkey = VK_SHIFT;
                chm.scancode = chm.chr == 0xF1 ? 0x2A : 0x36;
                chm.chr = 0;
                state |= Qt::AltModifier | Qt::ShiftModifier;
                // code will be assigned by the normal procedure below
            }
        } else if (chm.vkey == VK_ALTGRAF) {
            if (!(chm.fs & KC_KEYUP))
                mExtraKeyState |= Qt::AltModifier;
            else
                mExtraKeyState &= ~Qt::AltModifier;
        }
        if (chm.vkey < sizeof(KeyTbl))
            code = KeyTbl[chm.vkey];
    }

    // Detect some special keys that don't have a virtual key but have a pseudo
    // char code in the high byte of chm.chr (probably this is less
    // device-dependent than scancode).
    if (!code){
        switch (chm.chr) {
            case 0xEC00: // LWIN
            case 0xED00: // RWIN
                code = Qt::Key_Meta;
                if (!(chm.fs & KC_KEYUP))
                    mExtraKeyState |= Qt::MetaModifier;
                else
                    mExtraKeyState &= ~Qt::MetaModifier;
                break;
            case 0xEE00: // WINAPP (menu with arrow)
                code = Qt::Key_Menu;
                break;
            case 0x5600: // additional '\' (0x56 is actually its scancode)
                chm.chr = state & Qt::ShiftModifier ? '|' : '\\';
                break;
            case 0xFA00: // Back
                code = Qt::Key_Back;
                break;
            case 0xF900: // Forward
                code = Qt::Key_Forward;
                break;
            case 0x2064: // Volume Mute
                code = Qt::Key_VolumeMute;
                break;
            case 0x2E63: // Volume Down
                code = Qt::Key_VolumeDown;
                break;
            case 0x3062: // Volume Up
                code = Qt::Key_VolumeUp;
                break;
            case 0x2267: // Play/Pause
                code = Qt::Key_MediaPlay;
                break;
            case 0x326D: // Web/Home
                code = Qt::Key_HomePage;
                break;
            case 0xF500: // Search
                code = Qt::Key_Search;
                break;
            case 0xF600: // Favorites
                code = Qt::Key_Favorites;
                break;
        }
    }

    // Update state after updating mExtraKeyState.
    if (mExtraKeyState & Qt::AltModifier)
        state |= Qt::AltModifier;
    if (mExtraKeyState & Qt::MetaModifier)
        state |= Qt::MetaModifier;

    // Invert state logic:
    // If the key actually pressed is a modifier key, then we remove its modifier key from the
    // state, since a modifier-key can't have itself as a modifier
    //
    // ### QKeyEvent::modifiers() already does this inversion which in result
    // cancels the inversion we do below and therefore makes the above statement
    // incorrect! It looks like the inversion block should be removed from this
    // function but it is left here for compatibility with other platforms which
    // also have this bug.
    //
    if (code == Qt::Key_Control)
        state = state ^ Qt::ControlModifier;
    else if (code == Qt::Key_Shift)
        state = state ^ Qt::ShiftModifier;
    else if (code == Qt::Key_Alt)
        state = state ^ Qt::AltModifier;
    else if (code == Qt::Key_Meta)
        state = state ^ Qt::MetaModifier;

    bool isNumLockOn = WinGetKeyState(HWND_DESKTOP, VK_NUMLOCK) & 0x0001;

    // Detect numeric keypad keys.
    if (chm.vkey == VK_ENTER || chm.vkey == VK_NUMLOCK) {
        // These always come from the numpad.
        state |= Qt::KeypadModifier;
    } else if (((chm.vkey >= VK_PAGEUP && chm.vkey <= VK_DOWN) ||
                chm.vkey == VK_INSERT || chm.vkey == VK_DELETE)) {
        // These are numbers 0-9 and the numeric comma/dot.
        if ((chm.chr & 0xFF) != 0xE0) {
            state |= Qt::KeypadModifier;
            if ((state & Qt::AltModifier) && chm.vkey != VK_DELETE) {
                // Hide the key from Qt widgets and let the standard OS/2 Alt+ddd
                // shortcut (that composed chars from typed in ASCII codes) work
                // correctly. If we don't do that, widgets will see both individual
                // digits (or cursor movements if NumLock is off) and the char
                // composed by Alt+ddd.
                return false;
            }
            if (state & (Qt::KeyboardModifierMask & ~Qt::KeypadModifier)) {
                // The OS/2 keyboard driver tends to invert the NumLock state
                // when some modifiers (e.g. Shift) are pressed so that cursor
                // movement event becomes a digit and and vice versa; we want
                // to cancel replacing movements with digits to make it possible
                // to e.g. select the text using numpad keys in NumLock OFF mode
                // with Shift pressed. It seems that other applications (e.g.
                // standard PM controls) do exactly the same.
                if (!isNumLockOn) {
                    // Suppress generation of digits.
                    chm.chr = 0;
                    chm.fs &= ~KC_CHAR;
                }
            }
        }
    }
    // Detect other numpad keys. OS/2 doesn't assign virtual keys to them
    // so use scancodes (it can be device-dependent, is there a better way?)
    switch (chm.scancode) {
        case 0x4C: // 5
            state |= Qt::KeypadModifier;
            if (state & Qt::AltModifier) {
                // Hide the key from Qt (see above).
                return false;
            }
            if (state & (Qt::KeyboardModifierMask & ~Qt::KeypadModifier)) {
                if (!isNumLockOn) {
                    // Suppress generation of digits, see above (note that
                    // standard PM applications don't seem to do that for the
                    // "5" key; we correct it here as it seems more logical).
                    chm.chr = 0;
                    chm.fs &= ~KC_CHAR;
                }
            }
            // Scancode is zero if Numlock is set.
            if (!code)
                code = Qt::Key_Clear;
            // This key doesn't have a virtual key assigned, but pretend we're
            // a virtual key to avoid interpreting chm.chr as DBCS below.
            chm.fs |= KC_VIRTUALKEY;
            break;
        case 0x37: // *
            // OS/2 assigns VK_PRINTSCRN to it when pressed with Shift, also
            // it sets chr to zero when it is released with Alt or Ctrl
            // leaving vkey as zero too, and does few other strange things --
            // override them all.
            code = Qt::Key_Asterisk;
            state |= Qt::KeypadModifier;
            break;
        case 0x5C: // /
            code = Qt::Key_Slash;
            // Fall through.
        case 0x4A: // -
        case 0x4E: // +
            // The code for the above two is obtained by KbdXlate above.
            state |= Qt::KeypadModifier;
            break;
    }

    if ((state & Qt::KeypadModifier) && (chm.fs & KC_CHAR) &&
        chm.vkey != VK_ENTER) {
        // If it's a numpad key and it also provides a character, it means
        // Numlock is on. According to other platforms, key code must be the
        // character value in this case. Reset code to 0 to have it done so.
        // Enter is an exception as it should always produce a virtual code.
        if (chm.fs & KC_CHAR)
            code = 0;
    }

    qCDebug(lcQpaEvents) << "FOCUS object" << QGuiApplication::focusObject() << "window" << QGuiApplication::focusWindow();

    // Note: code and/or chm.scancode may be zero here. We cannot ignore such
    // events because, for example, all non-ASCII letters have zero virtual
    // codes, and DBCS characters entered via IME have both zero virtual codes
    // and zero scancodes. However, if both code and chm.scancode are zero
    // (as for DBCS), storeKey()/findKey() will do nothing which means that:
    //
    //   1) QKeyEvents will not have the auto-repeat flag set when a key is
    //      being auto-repeated by the system;
    //   2) there will be no QEvent::KeyRelease event corresponding to the
    //      QEvent::KeyPress event.
    //
    // This seems to be acceptable.

    // Note 2: Some events will lead to cases when there is no code and no text - one example is
    // a lid open event on Asus K75j that generates KC_SCANCODE with scancode set to 0x68 and
    // char set to 0xEF00. There is no Key_ constant for such an event so there is nothing to
    // translate it to in the above switch. It makes no sense to report such events to Qt as
    // they carry no information at all and may confuse applications (e.g. tests/manual/windowflags
    // that crashes in such a case). So we completely ignore them in the KEYDOWN code.

    // KEYDOWN -----------------------------------------------------------------
    if (!(chm.fs & KC_KEYUP)) {
        // Get the last record of this key press, so we can validate the current state
        // The record is not removed from the list
        KeyRecord *rec = KeyRecorder.findKey(chm.scancode, false);

        if (state == Qt::AltModifier) {
            // Special handling of global PM hotkeys.
            switch (code) {
                case Qt::Key_Space: {
                    // Show system menu.
                    HWND fId = window->mainHwnd();
                    HWND sysMenu = WinWindowFromID(fId, FID_SYSMENU);
                    if (!sysMenu)
                        break; // no menu for this window
                    WinPostMsg(sysMenu, MM_STARTMENUMODE,
                               MPFROM2SHORT(TRUE, FALSE), 0);
                    return true;
                }
                case Qt::Key_F4: {
                    // We handle this key combination ourselves because not
                    // all top-level widgets have the system menu.
                    WinPostMsg(window->mainHwnd(),
                               WM_CLOSE, 0, 0);
                    return true;
                }
                default:
                    break;
            }
        }

        // If rec's state doesn't match the current state, something has changed behind our back
        // (Consumed by modal widget is one possibility) So, remove the record from the list
        // This will stop the auto-repeat of the key, should a modifier change, for example
        if (rec && rec->state != state) {
            KeyRecorder.findKey(chm.scancode, true);
            rec = 0;
        }

        // If we have a record, it means that the key is already pressed, the state is the same
        // so, we have an auto-repeating key
        if (rec) {
            Q_ASSERT(!code || code == rec->code);
            if (rec->code < Qt::Key_Shift || rec->code > Qt::Key_ScrollLock) {
                qCDebug(lcQpaEvents) << "KEY REPEAT" << DV(receiver) << DV(rec->code) << Qt::KeyboardModifiers(state) << DV(rec->text);
                k0 = QWindowSystemInterface::handleExtendedKeyEvent(receiver, QEvent::KeyRelease, rec->code,
                                                                    Qt::KeyboardModifier(state), chm.scancode, chm.vkey, nativeMods,
                                                                    rec->text, true);
                k1 = QWindowSystemInterface::handleExtendedKeyEvent(receiver, QEvent::KeyPress, rec->code,
                                                                    Qt::KeyboardModifier(state), chm.scancode, chm.vkey, nativeMods,
                                                                    rec->text, true);
            }
        }
        // No record of the key being previous pressed, so we now send a QEvent::KeyPress event,
        // and store the key data into our records.
        else {
            QString text;
            // We assume that we have a correct 8-bit character not only when
            // KC_CHAR is set but also when there are one or more modifiers
            // (in which case KC_CHAR is not present) but no KC_VIRTUALKEY.
            // The latter is in particular important for Alt+Letter combos
            // processing Alt+Letter shortcuts for non-ASCII letters in widgets
            // (e.g. QPushButton) depends on that.
            if (((chm.fs & KC_CHAR) ||
                 (!(chm.fs & KC_VIRTUALKEY) && (chm.fs & (KC_CTRL | KC_ALT | KC_SHIFT)))) &&
                chm.chr) {
                if (chm.chr & 0xFF00) {
                    // We assume we get a DBCS char if the above condition is met.
                    text = QString::fromLocal8Bit((char *)&chm.chr, 2);
                } else {
                    text = QString::fromLocal8Bit((char*)&chm.chr, 1);
                }

                if (chm.fs & KC_DEADKEY) {
                    // Convert dead keys to Key_Dead_* codes and set text to
                    // null to avoid interpreting them as normal chars.
                    Q_ASSERT(text.size() == 1);
                    code = DeadCharToDeadKeyCode(text[0]);
                    text = QString();
                } else if (chm.fs & KC_INVALIDCOMP) {
                    // If the pressed letter is invalid for the given dead
                    // key, we set text to null as well (to meet the PM
                    // behavior) and set the code to the uppercase unicode
                    // value (similar to other cases) to have it recognized.
                    Q_ASSERT(text.size() == 1);
                    code = text[0].toUpper().unicode();
                    text = QString();
                }

                Q_ASSERT(code || !text.isEmpty()); // We must provide the key code.
                if (!code && !text.isEmpty()) {
                    code = text[0].toUpper().unicode();
                }

                if ((state & Qt::ControlModifier) &&
                    !(state & Qt::KeypadModifier) && !(state & Qt::AltModifier) &&
                    !text.isEmpty() && !text[0].row()) {
                    // Ctrl + A..Z etc. produce ascii from 0x01 to 0x1F
                    int ascii = text[0].toUpper().cell();
                    if (ascii >= 0x41 && ascii <= 0x5F)
                        ascii -= 0x40;
                    // The below emulates OS/2 functionality. It differs from
                    // e.g. Win32.
                    else if (ascii == 0x36 && !(state & Qt::KeypadModifier))
                        ascii = 0x1E;
                    else if (ascii == 0x2D)
                        ascii = 0x1F;
                    else if (ascii >= 0x7B && ascii <= 0x7D)
                        ascii -= 0x60;
                    text = QChar(ascii);
                }
            }
            if (!code && text.isEmpty()) {
                // We don't have any code or text here, meaning that this event is basically about
                // nothing, ignore it completely.
                qCDebug(lcQpaEvents) << "KEY EMPTY";
            } else {
                KeyRecorder.storeKey(chm.scancode, code, state, text);
                qCDebug(lcQpaEvents) << "KEY PRESS" << DV(receiver) << DV(code) << Qt::KeyboardModifiers(state) << DV(text);
                k0 = QWindowSystemInterface::handleExtendedKeyEvent(receiver, QEvent::KeyPress, code,
                                                                Qt::KeyboardModifier(state), chm.scancode, chm.vkey, nativeMods,
                                                                text, false);
            }
        }
    }
    // KEYUP -------------------------------------------------------------------
    else {
        // Try to locate the key in our records, and remove it if it exists.
        // The key may not be in our records if, for example, the down event was handled by
        // PM natively, or our window gets focus while a key is already press, but now gets
        // the key release event.
        KeyRecord* rec = KeyRecorder.findKey(chm.scancode, true);
        if (!rec) {
            // Someone ate the key down event
        } else {
            qCDebug(lcQpaEvents) << "KEY RELEASE" <<  DV(receiver) << DV(rec->code) << Qt::KeyboardModifiers(state) << DV(rec->text);
            k0 = QWindowSystemInterface::handleExtendedKeyEvent(receiver, QEvent::KeyRelease, rec->code,
                                                                Qt::KeyboardModifier(state), chm.scancode, chm.vkey, nativeMods,
                                                                rec->text, false);
        }
    }

    qCDebug(lcQpaEvents) << DV(k0) << DV(k1);

    // Return true if a QKeyEvent was sent to a widget.
    return k0 || k1;
}

Qt::KeyboardModifiers QOS2KeyMapper::queryKeyboardModifiers()
{
    // TODO: As opposed to QGuiApplication::keyboardModifiers(), this function should return
    // real, asynchronous key modifier states. We should use WinGetPhysKeyState for that but the
    // problem is that it requires scan codes which are not documented and may be system-dependent.
    // Use a synchronous version for now.

    Qt::KeyboardModifiers modifiers = Qt::NoModifier;

    if (WinGetKeyState(HWND_DESKTOP, VK_SHIFT ) & 0x8000)
        modifiers |= Qt::ShiftModifier;
    if ((WinGetKeyState(HWND_DESKTOP, VK_ALT) & 0x8000) || (mExtraKeyState & Qt::AltModifier))
        modifiers |= Qt::AltModifier;
    if (WinGetKeyState(HWND_DESKTOP, VK_CTRL) & 0x8000)
        modifiers |= Qt::ControlModifier;
    if (mExtraKeyState & Qt::MetaModifier)
        modifiers |= Qt::MetaModifier;

    return modifiers;
}

QList<int> QOS2KeyMapper::possibleKeys(const QKeyEvent *e) const
{
    QList<int> result;

    const KeyboardLayoutItem &kbItem = mKeyLayout[e->nativeScanCode()];
    if (!kbItem.layoutIds[0]) {
        qCDebug(lcQpaEvents) << "none";
        return result;
    }

    int baseKey0 = kbItem.qtKey[0][0] ? kbItem.qtKey[0][0] :
                   e->key() && e->key() != Qt::Key_unknown ? e->key() :
                   e->text().at(0).unicode();

    int baseKey1 = baseKey0;
    if (kbItem.layoutIds[1])
        baseKey1 = kbItem.qtKey[0][1] ? kbItem.qtKey[0][1] :
                   e->key() && e->key() != Qt::Key_unknown ? e->key() :
                   e->text().at(0).unicode();

    Qt::KeyboardModifiers keyMods = e->modifiers();

    // The base key is _always_ valid, of course.
    result << int(baseKey0 + keyMods);
    if (baseKey1 != baseKey0)
        result << int(baseKey1 + keyMods);

    // Go through both keyboard layouts
    for (int j = 0; j < 2; ++j) {
        // Check if we skipped the layout in updateKeyMap() and skip too if so.
        if (!kbItem.layoutIds[j])
            continue;
        // Go through all modifiers.
        for(int i = 0; i < KeyboardLayoutItem::QtKeySize; ++i) {
            Qt::KeyboardModifiers neededMods = QtModsTbl[i];
            int key = kbItem.qtKey[i][j];
            if (key && key != baseKey0 && key != baseKey1 &&
                ((keyMods & neededMods) == neededMods)) {
                int k = int(key + (keyMods & ~neededMods));
                if (!result.contains(k))
                    result << k;
            }
        }
    }

    qCDebug(lcQpaEvents) << Qt::hex << result;

    return result;
}

void QOS2KeyMapper::updateKeyMap(HWND hwnd, CHRMSG &chm)
{
    // It may be a keyboard layout change message, see translateKeyEvent()
    // for details.
    if ((chm.fs & KC_VIRTUALKEY) && chm.vkey == 0) {
        if (chm.chr == 0xF0 || chm.chr == 0xF1) {
            chm.fs |= KC_ALT | KC_SHIFT;
            chm.vkey = VK_SHIFT;
            chm.scancode = chm.chr == 0xF1 ? 0x2A : 0x36;
            chm.chr = 0;
        }
    }

    if (!chm.scancode)
        return;

    KBDTRANS kt;
    ULONG curLayerId = WinQueryKbdLayer(hwnd);

    int layoutsChanged = 0;

    // Go through both keyboard layouts.
    for (int j = 0; j < 2; ++j) {
        WinSetKbdLayer(hwnd, j ? KL_NATIONAL : KL_LATIN, 0);
        // Check if the data is still valid and skip if so. Also skip the
        // National layout if it's the same as Latin.
        ULONG layoutId = WinQueryKbdLayout(HWND_DESKTOP);
        if (mKeyLayout[chm.scancode].layoutIds[j] == layoutId ||
            (j && mKeyLayout[chm.scancode].layoutIds[0] == layoutId))
            continue;
        mKeyLayout[chm.scancode].layoutIds[j] = layoutId;

        // Now go through all modifiers.
        for (int i = 0; i < KeyboardLayoutItem::QtKeySize; ++i) {
            // Reset all kbd states and modifiers.
            memset(&kt, 0, sizeof(KBDTRANS));
            kt.chScan = chm.scancode;
            kt.fsState = KbdModsTbl[i];
            APIRET arc = KbdXlate(&kt, 0);
            // TODO: KbdXlate is known to fail with ERROR_INVALID_HANDLE or ERROR_BAD_COMMAND when
            // the calling process is run under SSHD by nickk. Ignore these errors for now (the
            // worst that'll happen is broken QShortcutMap magic (and some shortcut functionality).
            // It's better than aborting on Q_ASSERT.
            if (arc == NO_ERROR)
                mKeyLayout[chm.scancode].qtKey[i][j] =
                    QString::fromLocal8Bit((char*)&kt.chChar, 1)[0].toUpper().unicode();
        }
        if (Q_UNLIKELY(lcQpaEventsDebug))
            ++layoutsChanged;
    }

    // Restore the layout.
    WinSetKbdLayer(hwnd, curLayerId, 0);

    if (Q_UNLIKELY(lcQpaEventsDebug) && layoutsChanged) {
        qCDebug(lcQpaEvents, "scancode 0x%02x (layoutIds: %ld, %ld)", chm.scancode,
                mKeyLayout[chm.scancode].layoutIds[0], mKeyLayout[chm.scancode].layoutIds[1]);
        for (int i = 0; i < KeyboardLayoutItem::QtKeySize; ++i) {
            qCDebug(lcQpaEvents, "    [%d] (0x%04x, '%lc') (0x%04x, '%lc')", i,
                    mKeyLayout[chm.scancode].qtKey[i][0],
                    mKeyLayout[chm.scancode].qtKey[i][0] < 0x20 ? ' ' :
                    mKeyLayout[chm.scancode].qtKey[i][0],
                    mKeyLayout[chm.scancode].qtKey[i][1],
                    mKeyLayout[chm.scancode].qtKey[i][1] < 0x20 ? ' ' :
                    mKeyLayout[chm.scancode].qtKey[i][1]);
        }
    }
}

QT_END_NAMESPACE
