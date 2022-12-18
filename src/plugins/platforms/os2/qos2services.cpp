/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
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

#define QT_NO_URL_CAST_FROM_STRING

#include "qos2services.h"
#include <QtCore/qurl.h>
#include <QtCore/qdebug.h>
#include <QtCore/qdir.h>
#include <QtCore/qscopedpointer.h>
#include <QtCore/qstring.h>
#include <QtCore/qthread.h>

QT_BEGIN_NAMESPACE

static bool launch(const char* str, int type)
{
    CHAR DefaultExe[CCHMAXPATH] = {0};
    CHAR DefaultDir[CCHMAXPATH] = {0};
    CHAR DefaultParam[CCHMAXPATH] = {0};
    CHAR*     p;
    const QDir curdir = QDir::current();
    bool dirchanged = false;
    QDir newdir = QDir::root();

    // Browser or Mailto: link
    if (type == 0 || type ==1) {
        ulong size = sizeof(DefaultExe);
        PrfQueryProfileData(HINI_USERPROFILE, "WPURLDEFAULTSETTINGS",
                            (type == 0 ? "DefaultBrowserExe" : "DefaultMailExe"), DefaultExe, &size);
        if (!DefaultExe) {
            if (type == 0)
                qWarning("A default browser is not designated in WPURLDEFAULTSETTINGS->DefaultBrowserExe in OS2.INI");
            else if (type == 1)
                qWarning("A default mail app is not designated in WPURLDEFAULTSETTINGS->DefaultMailExe in OS2.INI");
            return false;
        }
        size = sizeof(DefaultDir);
        PrfQueryProfileData(HINI_USERPROFILE, "WPURLDEFAULTSETTINGS",
                            (type == 0 ? "DefaultWorkingDir" : "DefaultMailWorkingDir"), DefaultDir, &size);
        // Set the directory
        if (DefaultDir) { 
            QString dirstr = QString::fromLatin1(DefaultDir);
            if (newdir.setCurrent(dirstr)) {
                dirchanged = true;
            }
            else
                qWarning("Cannot find the default working directory");
        }
        // Mailto:
        if (type == 1) {
            size = sizeof(DefaultParam);
            PrfQueryProfileData(HINI_USERPROFILE, "WPURLDEFAULTSETTINGS",
                                "DefaultMailParameters", DefaultParam, &size);
            strcat(DefaultParam, " ");
            strcat(DefaultParam, str);
        }
    }
    //WinQueryObject doesn't work with forward slashes
    if (strstr(str, "file:///")) {
        strcpy((char*) str, str + 8);
        p = (char*) str;
        while (*p) {
          if (*p == '/')
            *p = '\\';
          p++;
        }
    }
    //Executable file passed Not sure this happens
    if (type == 3) { 
        p = strchr(str, ' ');
        p++;
        strcpy(DefaultParam, p);
        p--;
        *p = 0;
        qWarning("exe %s param %s\n", str, DefaultParam);
    }
    //Filename passed
    if (type == 2) {
        HOBJECT hObject = WinQueryObject(str);
        if (hObject != 0)
            if (WinOpenObject(hObject, 0 /*OPEN_DEFAULT*/, TRUE))
                return true;
        qWarning("WinOpenObject failed");
    }
    else {
        PID       pid            = 0;
        ULONG     ulSessID       = 0;
        CHAR      achObjBuf[256] = {0};
        APIRET    rc             = NO_ERROR;
        STARTDATA startData;

        memset(&startData, 0, sizeof(STARTDATA));
        startData.Length     = sizeof(STARTDATA);
        startData.Related    = SSF_RELATED_INDEPENDENT;
        startData.InheritOpt = SSF_INHERTOPT_PARENT;
        startData.FgBg       = SSF_FGBG_FORE; 
        startData.PgmControl = SSF_CONTROL_VISIBLE;
        startData.TraceOpt   = SSF_TRACEOPT_NONE;
        startData.PgmTitle   = NULL;
        startData.PgmName    = (type == 3 ? (PSZ) str : DefaultExe);
        startData.TermQ      = 0;
        startData.Environment = NULL;
        startData.PgmInputs = (type == 0 ? (PBYTE) str : DefaultParam);
        startData.SessionType = SSF_TYPE_PM;
        startData.ObjectBuffer  = achObjBuf;
        startData.ObjectBuffLen = (ULONG) sizeof(achObjBuf);

        rc = DosStartSession(&startData, &ulSessID, &pid);

        if (dirchanged) {
            //Set the directory to root since we might have changed drives for the working directory
            QString temp = newdir.current().absolutePath();

            temp.truncate(3);
            newdir.setCurrent(temp);
            //Reset the current directory to the original
            if (!newdir.setCurrent(curdir.absolutePath()))
                qWarning("Reseting the current directory failed");
        }
        if (!rc) {
            return true;
        }
        else {
            qWarning("DosStartSession err %i \n", (int) rc);
        }
    }
    return false;
}

bool QOS2Services::openUrl(const QUrl &url)
{
    int type = 0;
    const QString scheme = url.scheme();
    if (scheme == u"mailto")
        type = 1;
    return launch((const char *) url.toString().toLatin1(), type);
}
bool QOS2Services::openDocument(const QUrl &url)
{
    int type = 2;
    if (url.toLocalFile().contains(".exe", Qt::CaseInsensitive))
        type = 3;
    return launch((const char *) url.toString().toLatin1(), type);
}

QT_END_NAMESPACE
