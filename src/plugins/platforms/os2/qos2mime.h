/****************************************************************************
**
** Copyright (C) 2021 bww bitwise works GmbH.
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

#ifndef QOS2MIME_H
#define QOS2MIME_H

#include <QtCore/qt_os2.h>

#include <QtCore/qlist.h>
#include <QtCore/qvariant.h>

QT_BEGIN_NAMESPACE

class QDebug;
class QMimeData;

class QOS2Mime
{
    friend class QOS2ClipboardData;
    friend class QOS2ClipboardRetrievalMimeData;

    Q_DISABLE_COPY_MOVE(QOS2Mime)
public:
    QOS2Mime();
    virtual ~QOS2Mime();

    struct MimeCFPair
    {
        MimeCFPair(const QString &m, ULONG f) : mime(m), format(f) {}
        QString mime;
        ULONG format;
    };

    // for converting from Qt
    virtual QList<MimeCFPair> formatsForMimeData(const QMimeData *mimeData) const = 0;
    virtual bool convertFromMimeData(const QMimeData *mimeData, ULONG format,
                                     ULONG &flags, ULONG *data) const = 0;
    // for converting to Qt
    virtual QList<MimeCFPair> mimesForFormats(const QList<ULONG> &formats) const = 0;
    virtual QVariant convertFromFormat(ULONG format, ULONG flags, ULONG data,
                                       const QString &mimeType,
                                       QVariant::Type preferredType) const = 0;

protected:

    static ULONG registerMimeType(const QString &mime);
    static void unregisterMimeType(ULONG mimeId);

    static QList<QOS2Mime*> all();

    static ULONG allocateMemory(size_t size);
    static void freeMemory(ULONG addr);

    static QString formatName(ULONG format);

private:
    struct Match
    {
        Match(QOS2Mime *c, const QString f, ULONG cf, int p) :
            converter(c), mime(f), format(cf), priority(p) {}

        QOS2Mime *converter;
        QString mime;
        ULONG format;
        int priority;
    };

    static QList<Match> allConvertersFromFormats(const QList<ULONG> &formats);
    static QList<Match> allConvertersFromMimeData(const QMimeData *mimeData);
};

QT_END_NAMESPACE

#endif // QOS2MIME_H
