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

#include "qos2context.h"

#include "qos2backingstore.h"
#include "qos2screen.h"
#include "qos2window.h"

#include <QtGui/qpainter.h>

QT_BEGIN_NAMESPACE

namespace {

const bool lcQpaBackingStoreInfo = lcQpaBackingStore().isInfoEnabled();
const bool lcQpaBackingStoreDebug = lcQpaBackingStore().isDebugEnabled();

} // unnamed namespace

QOS2BackingStore::QOS2BackingStore(QWindow *window)
    : QPlatformBackingStore(window)
{
    qCInfo(lcQpaBackingStore) << DV(window);
}

QOS2BackingStore::~QOS2BackingStore()
{
}

QPaintDevice *QOS2BackingStore::paintDevice()
{
    return &mImage;
}

void QOS2BackingStore::flush(QWindow *window, const QRegion &region, const QPoint &offset)
{
    QOS2Window *os2window = static_cast<QOS2Window *>(window->handle());
    Q_ASSERT(os2window);

    HPS hps = os2window->acquirePs();
    QRect br = region.boundingRect();
    QRect wr = os2window->geometry();

    qCInfo(lcQpaBackingStore) << window << region << offset << os2window << Qt::hex << DV(hps);

    // Use the reflection + transformation matrix to flip the y axis. This is
    // proven to be much faster than manual image flipping on the real video
    // hardware as it probably involves some hardware acceleration in the video
    // driver.

    MATRIXLF m;
    m.fxM11 = MAKEFIXED(1, 0);
    m.fxM12 = 0;
    m.lM13 = 0;
    m.fxM21 = 0;
    m.fxM22 = MAKEFIXED(-1, 0);
    m.lM23 = 0;
    m.lM31 = 0;
    m.lM32 = wr.height() - 1;
    GpiSetDefaultViewMatrix(hps, 8, &m, TRANSFORM_REPLACE);

    br.translate(offset);

    // Make sure br doesn't exceed the backing storage size (it may happen
    // during resize & move due to the different event order).
    br = br.intersected(QRect(0, 0, mImage.width(), mImage.height()));

    // Note: remove offset from wbr because the window's HPS has a proper
    // origin already that includes this offset (which is in fact a position of
    // the window relative to its top-level parent).
    QRect wbr = br.translated(-offset);

    BITMAPINFOHEADER2 bmh;
    memset(&bmh, 0, sizeof(BITMAPINFOHEADER2));
    bmh.cbFix = sizeof(BITMAPINFOHEADER2);
    bmh.cPlanes = 1;
    bmh.cBitCount = QOS2Screen::Depth();
    bmh.cx = mImage.width();
    bmh.cy = mImage.height();

    // Note: target is inclusive-inclusive, source is inclusive-exclusive
    POINTL ptls[] = { { wbr.left(), wbr.top() },
                      { wbr.right(), wbr.bottom() },
                      { br.left(), br.top() },
                      { br.right() + 1, br.bottom() + 1 } };

    GpiDrawBits(hps, (PVOID)mImage.bits(), (PBITMAPINFO2)&bmh, 4, ptls, ROP_SRCCOPY, BBO_IGNORE);

    if (Q_UNLIKELY(lcQpaBackingStoreInfo)) {
        // Display a flashing red rectangle around the update rect for debug purposes.
        RECTL rcl = { wbr.left(), wr.height() - wbr.bottom() - 1, wbr.right() + 1, wr.height() - wbr.top() };
        WinDrawBorder(hps, &rcl, 1, 1, CLR_RED, CLR_BACKGROUND, DB_PATCOPY);
        DosSleep(50);
        GpiDrawBits(hps, (PVOID)mImage.bits(), (PBITMAPINFO2)&bmh, 4, ptls, ROP_SRCCOPY, BBO_IGNORE);
    }

    os2window->releasePs(hps);

    if (Q_UNLIKELY(lcQpaBackingStoreDebug)) {
        // Write the image for debug purposes.
        static int n = 0;
        const QString fileName = QString::asprintf("hwnd_%08lx_%d.png", os2window->hwnd(), n++);
        mImage.save(fileName);
        qCDebug(lcQpaBackingStore) << "Wrote" << mImage.size() << fileName;
    }
}

void QOS2BackingStore::resize(const QSize &size, const QRegion &staticContents)
{
    if (mImage.size() != size) {
        QImage::Format format = QOS2Screen::Format();
        mImage = QImage(size, format);
    }

    qCInfo(lcQpaBackingStore) << size << staticContents << mImage;
}

void QOS2BackingStore::beginPaint(const QRegion &region)
{
    // Ideally, widgets requesting translucency should check whether the actual QWindow::format
    // supports it and act accordingly (e.g. paint the appropriate background themselves if not)
    // but in reality even Qt own widgets (e.g. QSplashScreen) don't do that. This forces us to
    // always check for the requested format and paint the image white if translucency is needed
    // (to avoid previous QImage data in non-opaque areas to appear on the screen).
    bool hasAlpha = window()->requestedFormat().hasAlpha();

    qCInfo(lcQpaBackingStore) << region << DV(hasAlpha);

    if (hasAlpha) {
        QPainter p(&mImage);
        const QColor blank = Qt::white;
        for (const QRect &r : region)
            p.fillRect(r, blank);
    }
}

QT_END_NAMESPACE
