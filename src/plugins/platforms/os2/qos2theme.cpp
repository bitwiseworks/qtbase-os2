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

#include "qplatformdefs.h" // for strupr

#include "qos2theme.h"

#include "qos2context.h"

#include <qpa/qplatformdialoghelper.h>

#include <QtGui/QGuiApplication>
#include <QtGui/QColor>
#include <QtGui/QFont>
#include <QtGui/QPalette>

QT_BEGIN_NAMESPACE

namespace {

QRgb qt_sysclr2qrgb(LONG sysClr)
{
    // QRgb has the same RGB format (0xaarrggbb) as OS/2 uses (ignoring the
    // highest alpha byte) so we just cast the OS/2 LONG RGB value to qRgb
    // which is an unsigned int actually.
    return ((QRgb)WinQuerySysColor(HWND_DESKTOP, sysClr, 0)) & RGB_MASK;
}

QFont *qt_sysfont2qfont(PCSZ scope)
{
    // note: 10.System Proportional is de-facto the default font selected into
    // the presentation space

    static PCSZ app = "PM_SystemFonts";
    static PCSZ def = "10.System Proportional";

    ULONG keyLen = 0;
    QFont *f = new QFont(QStringLiteral("System Proportional"), 10);

    if (PrfQueryProfileSize(HINI_USERPROFILE, app, scope, &keyLen) && keyLen) {
        keyLen++; // Reserve space for the dot.
        char *buf = new char[keyLen];
        ULONG realLen = PrfQueryProfileString(HINI_USERPROFILE, app, scope, def,
                                              buf, keyLen);
        realLen--; // Exclude terminating NULL.

        qCInfo(lcQpaWindows) << scope << buf;

        // Parse the font definition.
        int height = 0;
        char *dot = strchr(buf, '.'), *dot2 = 0;
        if (dot) {
            *dot = 0;
            height = strtoul(buf, NULL, 10);
            dot2 = strchr(++ dot, '.');
            if (dot2) {
                // process simulated styles
                buf[realLen] = '.';
                buf[realLen+1] = 0;
                strupr(dot2);
                // TODO: Currently, simulated bold and italic font styles are not
                // supported by Qt/OS2 because Qt doesn't support style simulation
                // explicitly. the code below is commented out to prevent selecting
                // true fonts when simulated ones are actually requested.
                // if (strstr(dot2, ".BOLD.")) f.setBold(true);
                // if (strstr(dot2, ".ITALIC.")) f.setItalic(true);
                if (strstr(dot2, ".UNDERSCORE.")) f->setUnderline(true);
                if (strstr(dot2, ".UNDERLINED.")) f->setUnderline(true);
                if (strstr(dot2, ".STRIKEOUT.")) f->setStrikeOut(true);
                *dot2 = 0;
            }

            // Query non-simulated styles.
            HPS hps = WinGetScreenPS(HWND_DESKTOP);
            FONTMETRICS fm;
            LONG cnt = 1; // use the first match
            GpiQueryFonts(hps, QF_PUBLIC, dot, &cnt, sizeof(FONTMETRICS), &fm);
            if (cnt) {
                if (fm.fsSelection & FM_SEL_ITALIC) f->setItalic(true);
                if (fm.fsType & FM_TYPE_FIXED) f->setFixedPitch(true);
                USHORT weight = fm.usWeightClass;
                if (weight < 4) f->setWeight(QFont::Light);
                else if (weight < 6) f->setWeight(QFont::Normal);
                else if (weight < 7) f->setWeight(QFont::DemiBold);
                else if (weight < 8) f->setWeight(QFont::Bold);
                else f->setWeight(QFont::Black);
                // TODO: WarpSans (workplace Sans) has width = 4 which matches QFont::SemiCondensed
                // below. However, returning this stretch for SystemFont to Qt somehow breaks font
                // selection by making many fonts (e.g. Helvetica) look condenced and positioning
                // glyphs of WarpSans below the baseline. It may be a bug of Qt or our fontconfig.
                // Ignore width for now (it's irrelevant for INI settings anyway as there is no
                // syntax to configure it).
#if 0
                USHORT width = fm.usWidthClass;
                switch (width) {
                    case 1: f->setStretch(QFont::UltraCondensed); break;
                    case 2: f->setStretch(QFont::ExtraCondensed); break;
                    case 3: f->setStretch(QFont::Condensed); break;
                    case 4: f->setStretch(QFont::SemiCondensed); break;
                    case 5: f->setStretch(QFont::Unstretched); break;
                    case 6: f->setStretch(QFont::SemiExpanded); break;
                    case 7: f->setStretch(QFont::Expanded); break;
                    case 8: f->setStretch(QFont::ExtraExpanded); break;
                    case 9: f->setStretch(QFont::UltraExpanded); break;
                    default: f->setStretch(QFont::Unstretched); break;
                }
#endif
                f->setFamily(QString::fromLocal8Bit(fm.szFamilyname));
                f->setPointSize(height);
            }
            WinReleasePS(hps);

            qCInfo(lcQpaWindows) << "gpi metrics" << fm.szFamilyname << fm.szFacename << height << fm.usWeightClass << fm.usWidthClass
                                 << Qt::hex << fm.fsSelection << fm.fsType;
        }

        delete[] buf;
    }

    qCInfo(lcQpaWindows) << *f;

    return f;
}

} // namespace unnamed

QOS2Theme::QOS2Theme()
{
    std::fill(mFonts, mFonts + NFonts, static_cast<QFont *>(0));
    std::fill(mPalettes, mPalettes + NPalettes, static_cast<QPalette *>(0));
    refresh();
}

QOS2Theme::~QOS2Theme()
{
    clearPalettes();
    clearFonts();
}

QVariant QOS2Theme::themeHint(QPlatformTheme::ThemeHint hint) const
{
    switch (hint) {
    case UseFullScreenForPopupMenu:
        return QVariant(true);
    case DialogButtonBoxLayout:
        return QVariant(QPlatformDialogHelper::WinLayout);
    case StyleNames:
        return QVariant(QStringList(QStringLiteral("windows")));
    case KeyboardScheme:
        return QVariant(int(WindowsKeyboardScheme));
    case ContextMenuOnMouseRelease:
        return QVariant(true);
    default:
        break;
    }
    return QPlatformTheme::themeHint(hint);
}

void QOS2Theme::clearPalettes()
{
    qDeleteAll(mPalettes, mPalettes + NPalettes);
    std::fill(mPalettes, mPalettes + NPalettes, static_cast<QPalette *>(0));
}

void QOS2Theme::refreshPalettes()
{
    qCInfo(lcQpaWindows) << DV(QGuiApplication::desktopSettingsAware());

    clearPalettes();
    if (!QGuiApplication::desktopSettingsAware())
        return;

    // Let QPallette calculate all colors automatically based on the
    // base PM window background color -- to be on the safe side in case if we
    // don't set some role explicitly below (like QPalette::AlternateBase).
    QPalette *pal = new QPalette(QColor(qt_sysclr2qrgb(SYSCLR_DIALOGBACKGROUND)));

    pal->setColor(QPalette::WindowText, QColor(qt_sysclr2qrgb(SYSCLR_WINDOWTEXT)));
    pal->setColor(QPalette::Window, QColor(qt_sysclr2qrgb(SYSCLR_DIALOGBACKGROUND)));
    pal->setColor(QPalette::ButtonText, QColor(qt_sysclr2qrgb(SYSCLR_MENUTEXT)));
    pal->setColor(QPalette::Button, QColor(qt_sysclr2qrgb(SYSCLR_BUTTONMIDDLE)));
    pal->setColor(QPalette::Light, QColor(qt_sysclr2qrgb(SYSCLR_BUTTONLIGHT)));
    pal->setColor(QPalette::Dark, QColor(qt_sysclr2qrgb(SYSCLR_BUTTONDARK)));
    pal->setColor(QPalette::Midlight, QColor((pal->light().color().red()    + pal->button().color().red()) / 2,
                                             (pal->light().color().green()  + pal->button().color().green()) / 2,
                                             (pal->light().color().blue()   + pal->button().color().blue()) / 2));
    pal->setColor(QPalette::Mid, QColor((pal->dark().color().red()    + pal->button().color().red())   / 2,
                                        (pal->dark().color().green()  + pal->button().color().green()) / 2,
                                        (pal->dark().color().blue()   + pal->button().color().blue())  / 2));
    // Note: SYSCLR_SHADOW often = SYSCLR_BUTTONDARK.
    pal->setColor(QPalette::Shadow, QColor(qt_sysclr2qrgb(SYSCLR_BUTTONDEFAULT)));
    pal->setColor(QPalette::Text, QColor(qt_sysclr2qrgb(SYSCLR_WINDOWTEXT)));
    pal->setColor(QPalette::Base, QColor(qt_sysclr2qrgb(SYSCLR_ENTRYFIELD)));
    pal->setColor(QPalette::BrightText, QColor(qt_sysclr2qrgb(SYSCLR_BUTTONLIGHT)));
    pal->setColor(QPalette::Highlight, QColor(qt_sysclr2qrgb(SYSCLR_HILITEBACKGROUND)));
    pal->setColor(QPalette::HighlightedText, QColor(qt_sysclr2qrgb(SYSCLR_HILITEFOREGROUND)));
    // These colors are not present in the PM system palette.
    pal->setColor(QPalette::Link, Qt::blue);
    pal->setColor(QPalette::LinkVisited, Qt::magenta);

    // Disabled colors.
    // Note: it should be SYSCLR_MENUDISABLEDTEXT but many styles use etched
    // appearance for disabled elements (in combination with QPalette::Light)
    // which gives weakly readable text. Make it somewhat darker.
    pal->setColor(QPalette::Disabled, QPalette::WindowText, QColor(qt_sysclr2qrgb(SYSCLR_BUTTONDARK)));
    pal->setColor(QPalette::Disabled, QPalette::ButtonText, QColor(qt_sysclr2qrgb(SYSCLR_BUTTONDARK)));
    pal->setColor(QPalette::Disabled, QPalette::Text, QColor(qt_sysclr2qrgb(SYSCLR_BUTTONDARK)));

    mPalettes[SystemPalette] = pal;

    // Special palete: menus.
    pal = new QPalette(*mPalettes[SystemPalette]);
    pal->setColor(QPalette::Window, QColor(qt_sysclr2qrgb(SYSCLR_MENU)));
    pal->setColor(QPalette::WindowText, QColor(qt_sysclr2qrgb(SYSCLR_MENUTEXT)));
    pal->setColor(QPalette::Highlight, QColor(qt_sysclr2qrgb( SYSCLR_MENUHILITEBGND)));
    pal->setColor(QPalette::HighlightedText, QColor(qt_sysclr2qrgb(SYSCLR_MENUHILITE)));

    mPalettes[MenuPalette] = pal;
    mPalettes[MenuBarPalette] = new QPalette(*pal);

    // Special palete: static widget text.
    pal = new QPalette(*mPalettes[SystemPalette]);
    QColor staticTextCol(qt_sysclr2qrgb( SYSCLR_WINDOWSTATICTEXT));
    pal->setColor(QPalette::WindowText, staticTextCol);
    pal->setColor(QPalette::Text, staticTextCol);

    mPalettes[LabelPalette] = pal;
    mPalettes[GroupBoxPalette] = new QPalette(*pal);
}

void QOS2Theme::clearFonts()
{
    qDeleteAll(mFonts, mFonts + NFonts);
    std::fill(mFonts, mFonts + NFonts, static_cast<QFont *>(0));
}

void QOS2Theme::refreshFonts()
{
    qCInfo(lcQpaWindows) << DV(QGuiApplication::desktopSettingsAware());

    clearFonts();
    if (!QGuiApplication::desktopSettingsAware())
        return;

    mFonts[SystemFont] = qt_sysfont2qfont("WindowText");
    mFonts[TitleBarFont] = qt_sysfont2qfont("WindowTitles");
    mFonts[MenuFont] = qt_sysfont2qfont("Menus");
    mFonts[MenuBarFont] = new QFont(*mFonts[MenuFont]);
    mFonts[ItemViewFont] = qt_sysfont2qfont("IconText");

    // FixedFont is a usual app requirement (checked by tst_QFontDatabase).
    // There is a standard "Courier" Type 1 font coming with every OS/2 install
    // but prefer fontcofig matching as this can be configured by the user.
    mFonts[FixedFont] = new QFont("monospace");
}

QT_END_NAMESPACE
