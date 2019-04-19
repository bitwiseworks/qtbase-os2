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

#include "qos2screen.h"

// OS/2 has only one screen.
QOS2Screen *QOS2Screen::sInstance = nullptr;

QOS2Screen::QOS2Screen()
{
    if (sInstance)
        qFatal("There may be only one instance of QOS2Screen");

    sInstance = this;

    mHps = WinGetScreenPS(HWND_DESKTOP);
    Q_ASSERT(mHps);

    mWidth = WinQuerySysValue(HWND_DESKTOP, SV_CXSCREEN);
    mHeight = WinQuerySysValue(HWND_DESKTOP, SV_CYSCREEN);

    // Query the native color format of the display (# of planes, bit count).
    LONG buf[2];
    BOOL brc = GpiQueryDeviceBitmapFormats(mHps, 2, buf);
    Q_ASSERT(brc);
    mDepth = buf[0] * buf[1];

    switch (mDepth) {
    case 32: mFormat = QImage::Format_RGB32; break;
    case 24: mFormat = QImage::Format_RGB888; break;
    case 16: mFormat = QImage::Format_RGB16; break;
    case 15: mFormat = QImage::Format_RGB555; break;
    case 8: mFormat = QImage::Format_Indexed8; break;
    case 1: mFormat = QImage::Format_MonoLSB; break;
    default: break;
    }

    qCInfo(lcQpaWindows) << "Screen size:" << mWidth << "x" <<  mHeight << "x" << mDepth << "format" << mFormat;
}

QOS2Screen::~QOS2Screen()
{
    WinReleasePS(mHps);
}

QPixmap QOS2Screen::grabWindow(WId /*winId*/, int /*x*/, int /*y*/, int /*width*/, int /*height*/) const
{
    return QPixmap();
}

QRect QOS2Screen::geometry() const
{
    return QRect(0, 0, mWidth, mHeight);
}

int QOS2Screen::depth() const
{
    return mDepth;
}

QImage::Format QOS2Screen::format() const
{
    return mFormat;
}

QPlatformCursor *QOS2Screen::cursor() const
{
    return nullptr;
}

QT_END_NAMESPACE
