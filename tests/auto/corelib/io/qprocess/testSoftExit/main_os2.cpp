/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** Copyright (C) 2018 bww bitwise works GmbH. OS/2 parts.
**
** This file is part of the test suite of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:GPL-EXCEPT$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3 as published by the Free Software
** Foundation with exceptions as appearing in the file LICENSE.GPL3-EXCEPT
** included in the packaging of this file. Please review the following
** information to ensure the GNU General Public License requirements will
** be met: https://www.gnu.org/licenses/gpl-3.0.html.
**
** $QT_END_LICENSE$
**
****************************************************************************/

#define INCL_BASE
#define INCL_PM
#include <os2.h>
#include <stdio.h>

int main()
{
    printf("Ready\n");
    fflush(stdout);

    // Dynamically switch ("morph") to PM mode if we have been
    // compiled as the console application.
    PPIB ppib;
    DosGetInfoBlocks(NULL, &ppib);
    if (ppib->pib_ultype != 3)
        ppib->pib_ultype = 3;

    // then create the message queue
    HAB hab = WinInitialize(0);
    WinCreateMsgQueue(hab, 0);

    QMSG msg;
    while (WinGetMsg(hab, &msg, NULLHANDLE, 0, 0)) {
        if (msg.msg == WM_CLOSE)
            WinPostMsg(NULLHANDLE, WM_QUIT, 0, 0);
    }

    return int(msg.mp1);
}
