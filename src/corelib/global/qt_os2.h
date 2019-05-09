/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** Copyright (C) 2018 bww bitwise works GmbH. OS/2 parts.
**
** This file is part of the QtCore module of the Qt Toolkit.
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

#ifndef QT_OS2_H
#define QT_OS2_H

#ifdef __EMX__
#define OS2EMX_PLAIN_CHAR
#endif

// Request everything we might ever need from including <os2.h>
#define INCL_BASE
#define INCL_PM
#include <os2.h>

#ifdef __cplusplus
extern "C" {
#endif

// Innotek GCC lacks some API functions in its version of OS/2 Toolkit headers

#define QCRGN_ERROR                 0
#define QCRGN_OK                    1
#define QCRGN_NO_CLIP_REGION        2

LONG APIENTRY WinQueryClipRegion(HWND hwnd, HRGN hrgnClip);

BOOL APIENTRY WinSetClipRegion(HWND hwnd, HRGN hrgnClip);

// Sent to a hwnd when the mouse pointer enters its rectangle
// (mp1 = hwnd that is entered, mp2 = hwnd that is left).
// FID_CLIENT also receives enter messages of its WC_FRAME.
#define WM_MOUSEENTER 0x41E

// Sent to a hwnd when the mouse pointer leaves its recrangle
// (mp1 = hwnd that is left, mp2 = hwnd that is entered).
// FID_CLIENT also receives leave messages of its WC_FRAME.
#define WM_MOUSELEAVE 0x41F

#ifdef __cplusplus
}
#endif

// Suppresses the ERROR_INTERRUPT return value in Dos API calls by retrying the
// operation as long as this code is returned. This is primarily intended to
// avoid unnecessary interrupts of system calls that may happen during POSIX
// signal delivery (e.g. SIGCHLD). Note that the macro cannot be used in
// assignment expressions so call it with the whole assignment as an argument.
#define qDosNI(expr) while((expr) == ERROR_INTERRUPT)

#endif // QT_OS2_H
