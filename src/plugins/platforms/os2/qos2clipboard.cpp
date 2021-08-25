/***************************************************************************
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

#include "qos2context.h"

#include "qos2clipboard.h"
#include "qos2mime.h"

#if QT_CONFIG(clipboard)

#include <QMimeData>

const bool lcQpaMimeDebug = lcQpaMime().isDebugEnabled();

class QOS2ClipboardRetrievalMimeData : public QInternalMimeData
{
public:
    bool hasFormat_sys(const QString &mimetype) const override;
    QStringList formats_sys() const override;
    QVariant retrieveData_sys(const QString &mimetype, QVariant::Type preferredType) const override;

private:
    bool peekData(bool leaveOpen = false) const;

    mutable QList<ULONG> formats;
    mutable QList<QOS2Mime::Match> matches;
};

bool QOS2ClipboardRetrievalMimeData::peekData(bool leaveOpen) const
{
    if (!WinOpenClipbrd(NULLHANDLE)) {
        qCWarning(lcQpaMime) << "WinOpenClipbrd failed with" << Qt::hex << WinGetLastError(NULLHANDLE);
        return false;
    }

    QList<ULONG> newFormats;
    ULONG cf = 0;
    while ((cf = WinEnumClipbrdFmts(NULLHANDLE, cf)))
        newFormats << cf;

    if (!leaveOpen)
        WinCloseClipbrd(NULLHANDLE);

    // optimization: we don't want to call the potentially expensive
    // allConvertersFromFormats() unlesss we really got a different set
    if (newFormats == formats)
        return true;

    formats = newFormats;
    matches = QOS2Mime::allConvertersFromFormats(formats);

    if (lcQpaMimeDebug)
    {
        foreach(ULONG cf, formats)
            qCDebug(lcQpaMime) << "have format" << Qt::hex << cf << QOS2Mime::formatName(cf).utf16();
        foreach(QOS2Mime::Match match, matches)
            qCDebug(lcQpaMime) << "converter" << match.converter << "mime" << match.mime.utf16() <<
                   "format" << match.format << "priority" << match.priority;
    }

    return true;
}

bool QOS2ClipboardRetrievalMimeData::hasFormat_sys(const QString &mime) const
{
    if (!peekData())
        return false;

    foreach (QOS2Mime::Match match, matches)
        if (match.mime == mime)
            return true;

    return false;
}

QStringList QOS2ClipboardRetrievalMimeData::formats_sys() const
{
    QStringList fmts;

    if (!peekData())
        return fmts;

    foreach (QOS2Mime::Match match, matches)
        fmts << match.mime;

    return fmts;
}

QVariant QOS2ClipboardRetrievalMimeData::retrieveData_sys(const QString &mime,
                                                          QVariant::Type type) const
{
    QVariant result;

    if (!peekData(true /*leaveOpen*/))
        return result;

    foreach (QOS2Mime::Match match, matches) {
        if (match.mime == mime) {
            ULONG flags;
            if (WinQueryClipbrdFmtInfo(NULLHANDLE, match.format, &flags)) {
                ULONG data = WinQueryClipbrdData(NULLHANDLE, match.format);
                result = match.converter->convertFromFormat(match.format, flags,
                                                            data, match.mime, type);
            }
            break;
        }
    }

    WinCloseClipbrd(NULLHANDLE);

    return result;
}

QOS2Clipboard::QOS2Clipboard()
    : mRetrievalData(new QOS2ClipboardRetrievalMimeData())
{
    qCInfo(lcQpaMime);
}

QOS2Clipboard::~QOS2Clipboard()
{
    qCInfo(lcQpaMime);

    delete mRetrievalData;
}

QMimeData *QOS2Clipboard::mimeData(QClipboard::Mode mode)
{
    qCInfo(lcQpaMime) << DV(mode);

    if (mode != QClipboard::Clipboard)
        return nullptr;

    return mRetrievalData;
}

void QOS2Clipboard::setMimeData(QMimeData *mimeData, QClipboard::Mode mode)
{
    qCInfo(lcQpaMime) << DV(mode) << mimeData;

    if (mode != QClipboard::Clipboard)
        return;
}

bool QOS2Clipboard::supportsMode(QClipboard::Mode mode) const
{
    return (mode == QClipboard::Clipboard);
}

bool QOS2Clipboard::ownsMode(QClipboard::Mode mode) const
{
    Q_UNUSED(mode);

    return false;
}

#endif
