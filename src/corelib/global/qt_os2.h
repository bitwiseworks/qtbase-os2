/****************************************************************************
**
** Copyright (C) 2010 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: Nokia Corporation (qt-info@nokia.com)
**
** Copyright (C) 2010 netlabs.org. OS/2 parts.
**
** This file is part of the QtCore module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial Usage
** Licensees holding valid Qt Commercial licenses may use this file in
** accordance with the Qt Commercial License Agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Nokia.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights.  These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
**
** If you have questions regarding the use of this file, please contact
** Nokia at qt-info@nokia.com.
** $QT_END_LICENSE$
**
****************************************************************************/

#ifndef QT_OS2_H
#define QT_OS2_H

#ifdef __EMX__
#define OS2EMX_PLAIN_CHAR
#endif
#define INCL_BASE
#define INCL_PM
#include <os2.h>

#if defined(__GNUC__) && defined(__INNOTEK_LIBC__)

#ifdef __cplusplus
extern "C" {
#endif

// wrappers for each Win* and Gpi* call that restore the FPU Control Word
#include <API_FPU_CW_Wrappers.h>

// Innotek GCC lacks some API functions in its version of OS/2 Toolkit headers

#define QCRGN_ERROR                 0
#define QCRGN_OK                    1
#define QCRGN_NO_CLIP_REGION        2

LONG APIENTRY WinQueryClipRegion(HWND hwnd, HRGN hrgnClip);

extern inline LONG APIENTRY __FPU_CW_WinQueryClipRegion (HWND hwnd, HRGN hrgnClip)
{
    unsigned int cw = __FPU_CW_Get();
    LONG ret = WinQueryClipRegion (hwnd, hrgnClip);
    __FPU_CW_Set(cw);
    return ret;
}
#define WinQueryClipRegion __FPU_CW_WinQueryClipRegion

BOOL APIENTRY WinSetClipRegion(HWND hwnd, HRGN hrgnClip);

extern inline LONG APIENTRY __FPU_CW_WinSetClipRegion (HWND hwnd, HRGN hrgnClip)
{
    unsigned int cw = __FPU_CW_Get();
    LONG ret = WinSetClipRegion (hwnd, hrgnClip);
    __FPU_CW_Set(cw);
    return ret;
}
#define WinSetClipRegion __FPU_CW_WinSetClipRegion

#ifdef __cplusplus
}
#endif

// Suppresses the ERROR_INTERRUPT return value in Dos API calls by retrying the
// operation as long as this code is returned. This is primarily intended to
// avoid unnecessary interrupts of system calls that may happen during POSIX
// signal delivery (e.g. SIGCHLD). Note that the macro cannot be used in
// assignment expressions so call it with the whole assignment as an argument.
#define qDosNI(expr) while((expr) == ERROR_INTERRUPT)

#endif // defined(Q_CC_GNU) && defined(__INNOTEK_LIBC__)

#endif // QT_OS2_H
