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

#include <QtCore/qthread.h>
#include <QtCore/private/qeventdispatcher_os2_p.h>

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

    if (lcQpaMimeDebug) {
        foreach(ULONG cf, formats)
            qCDebug(lcQpaMime) << "have format" << Qt::hex << cf << QOS2Mime::formatName(cf);
        foreach(QOS2Mime::Match match, matches)
            qCDebug(lcQpaMime) << "converter" << match.converter << "mime" << match.mime
                               << "format" << match.format << "priority" << match.priority;
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

class QOS2ClipboardData : public QPMObjectWindow
{
public:
    QOS2ClipboardData(QOS2Clipboard *q_);
    ~QOS2ClipboardData();

    void setSource(QMimeData *s);

    void setAsClipboardViewer();
    bool ownsClipboard() const;
    void putAllMimeToClipboard(bool isDelayed);
    void flushClipboard();

    QMimeData *mimeData();

private:
    bool setClipboard(QOS2Mime *converter, ULONG format, bool isDelayed);

    MRESULT message(ULONG msg, MPARAM mp1, MPARAM mp2);

    QOS2Clipboard *q;

    QMimeData *src;
    QList<QOS2Mime::Match> matches;
    HWND prevClipboardViewer;

    QOS2ClipboardRetrievalMimeData retrievalData;

    bool ignore_WM_DESTROYCLIPBOARD;
};

QOS2ClipboardData::QOS2ClipboardData(QOS2Clipboard *q_)
    : q(q_), src(0), prevClipboardViewer(NULLHANDLE)
    , ignore_WM_DESTROYCLIPBOARD(false)
{
    HWND clipboardViewer = WinQueryClipbrdViewer(NULLHANDLE);

    qCDebug(lcQpaMime) << Qt::hex << DV(hwnd()) << "cur viewer" <<  clipboardViewer;

    if (hwnd() != clipboardViewer) {
        prevClipboardViewer = clipboardViewer;
        BOOL ok = WinSetClipbrdViewer(NULLHANDLE, hwnd());
        if (!ok)
            qCWarning(lcQpaMime) << "WinSetClipbrdViewer failed with" << Qt::hex << WinGetLastError(NULLHANDLE);
    }
}

QOS2ClipboardData::~QOS2ClipboardData()
{
    qCDebug(lcQpaMime) << Qt::hex << DV(hwnd())
                       << "cur viewer" << WinQueryClipbrdViewer(NULLHANDLE)
                       << "prev viewer" << prevClipboardViewer
                       << "owner" << WinQueryClipbrdOwner(NULLHANDLE);

    flushClipboard();
    setSource(0);

    HWND clipboardViewer = WinQueryClipbrdViewer(NULLHANDLE);

    // make sure we are not the clipboard viewer any more (note that the viewer
    // may already be reset by some uninit code at this stage but we still need
    // to restore the previous one to let it do the job)
    if (hwnd() == clipboardViewer || clipboardViewer == NULLHANDLE)
        WinSetClipbrdViewer(NULLHANDLE, prevClipboardViewer);
}

void QOS2ClipboardData::setSource(QMimeData *s)
{
    if (s == src)
        return;
    delete src;
    src = s;

    // build the list of all mime <-> cf matches
    matches.clear();
    if (src)
        matches = QOS2Mime::allConvertersFromMimeData(src);

    if (lcQpaMimeDebug) {
        if (src) {
            qCDebug(lcQpaMime) << "mimes" << src->formats();
            foreach(QOS2Mime::Match match, matches)
                qCDebug(lcQpaMime) << "match: converter" << match.converter
                                   << "format" << Qt::hex << match.format
                                   << QOS2Mime::formatName(match.format) << Qt::dec
                                   << "priority" << match.priority;
        }
    }
}

bool QOS2ClipboardData::ownsClipboard() const
{
    return src && hwnd() == WinQueryClipbrdOwner(NULLHANDLE);
}

bool QOS2ClipboardData::setClipboard(QOS2Mime *converter, ULONG format,
                                     bool isDelayed)
{
    Q_ASSERT(src);
    if (!src)
        return false;

    bool ok, ok2;
    ULONG flags = 0, data = 0;

    if (isDelayed) {
        // setup delayed rendering of clipboard data
        ok = converter->convertFromMimeData(src, format, flags, 0);
        if (ok) {
            WinSetClipbrdOwner(NULLHANDLE, hwnd());
            ok2 = WinSetClipbrdData(NULLHANDLE, 0, format, flags);
        }
    } else {
        // render now
        ok = converter->convertFromMimeData(src, format, flags, &data);
        if (ok)
            ok2 = WinSetClipbrdData(NULLHANDLE, data, format, flags);
    }
    qCWarning(lcQpaMime) << "convert to format" << Qt::hex << format
                         << QOS2Mime::formatName(format)
                         << DV(flags) << DV(data) << Qt::dec
                         << DV(isDelayed) << DV(ok);
    if (ok && !ok2) {
        qCWarning(lcQpaMime) << "WinSetClipbrdData failed with" << Qt::hex << WinGetLastError(NULLHANDLE);
    }

    return ok && ok2;
}

void QOS2ClipboardData::putAllMimeToClipboard(bool isDelayed)
{
    qCWarning(lcQpaMime) << DV(isDelayed);

    if (!WinOpenClipbrd(NULLHANDLE)) {
        qCWarning(lcQpaMime) << "WinOpenClipbrd failed with" << Qt::hex << WinGetLastError(NULLHANDLE);
        return;
    }

    // delete the clipboard contents before we render everything to make sure
    // nothing is left there from another owner
    ignore_WM_DESTROYCLIPBOARD = true;
    BOOL ok = WinEmptyClipbrd(NULLHANDLE);
    ignore_WM_DESTROYCLIPBOARD = false;
    if (!ok) {
        qCWarning(lcQpaMime) << "WinEmptyClipbrd failed with" << Qt::hex << WinGetLastError(NULLHANDLE);
        WinCloseClipbrd(NULLHANDLE);
        return;
    }

    if (src) {
        foreach(QOS2Mime::Match match, matches)
            setClipboard(match.converter, match.format, isDelayed);
    }

    WinCloseClipbrd(NULLHANDLE);
}

void QOS2ClipboardData::flushClipboard()
{
    if (ownsClipboard()) {
        putAllMimeToClipboard(false);
        // make sure we won't be doing this again if asked in WM_RENDERALLFMTS
        setSource(0);
    }
}

QMimeData *QOS2ClipboardData::mimeData()
{
    // short cut for local copy / paste
    if (ownsClipboard())
        return src;
    return &retrievalData;
}

MRESULT QOS2ClipboardData::message(ULONG msg, MPARAM mp1, MPARAM mp2)
{
    qCDebug(lcQpaMime) << Qt::hex << DV(hwnd()) << DV(msg) << DV(mp1) << DV(mp2);

    switch (msg) {

        case WM_DRAWCLIPBOARD: {
            qCDebug(lcQpaMime) << "WM_DRAWCLIPBOARD:" << DV(src) << Qt::hex
                               << "cur viewer" << WinQueryClipbrdViewer(NULLHANDLE)
                               << "prev viewer" << prevClipboardViewer
                               << "owner" << WinQueryClipbrdOwner(NULLHANDLE);

            if (hwnd() != WinQueryClipbrdOwner(NULLHANDLE) && src) {
                // we no longer own the clipboard, clean up the clipboard object
                setSource(0);
            }

            // ask QClipboard to emit changed() signals
            q->emitChanged(QClipboard::Clipboard);

            // PM doesn't inform the previous clipboard viewer if another
            // app changes it (nor does it support viewer chains in some other
            // way). The best we can do is to propagate the message to the
            // previous clipboard viewer ourselves (though there is no guarantee
            // that all non-Qt apps will do the same).
            if (prevClipboardViewer) {
                // propagate the message to the previous clipboard viewer
                BOOL ok = WinPostMsg(prevClipboardViewer, msg, mp1, mp2);
                if (!ok)
                    prevClipboardViewer = NULLHANDLE;
            }
        }
        break;

        case WM_DESTROYCLIPBOARD: {
            qCDebug(lcQpaMime) << "WM_DESTROYCLIPBOARD";
            if (!ignore_WM_DESTROYCLIPBOARD)
                setSource(0);
        }
        break;

        case WM_RENDERFMT: {
            qCDebug(lcQpaMime) << "WM_RENDERFMT: CF" << Qt::hex << (ULONG)mp1
                               << QOS2Mime::formatName((ULONG)mp1);
            if (src) {
                foreach(QOS2Mime::Match match, matches) {
                    if (match.format == (ULONG)mp1) {
                        setClipboard(match.converter, match.format, false);
                        break;
                    }
                }
            }
        }
        break;

        case WM_RENDERALLFMTS: {
            qCDebug(lcQpaMime) << "WM_RENDERALLFMTS";
            if (src) {
                foreach(QOS2Mime::Match match, matches)
                    setClipboard(match.converter, match.format, false);
            }
        }
        break;

        default:
            break;
    }

    qCDebug(lcQpaMime) << "END";
    return FALSE;
}

QOS2Clipboard::QOS2Clipboard()
    : d(nullptr)
{
    qCInfo(lcQpaMime);
}

QOS2Clipboard::~QOS2Clipboard()
{
    qCInfo(lcQpaMime);

    if (d)
        delete data();
}

QMimeData *QOS2Clipboard::mimeData(QClipboard::Mode mode)
{
    qCInfo(lcQpaMime) << DV(mode);

    if (mode != QClipboard::Clipboard)
        return nullptr;

    return data()->mimeData();
}

void QOS2Clipboard::setMimeData(QMimeData *mimeData, QClipboard::Mode mode)
{
    qCInfo(lcQpaMime) << DV(mode) << mimeData;

    if (mode != QClipboard::Clipboard)
        return;

    QOS2ClipboardData *d = data();
    d->setSource(mimeData);

    if (!mimeData)
        return; // nothing to do

    // use delayed rendering only if the application runs the event loop
    bool runsEventLoop = QThread::currentThread()->loopLevel() != 0;
    qCDebug(lcQpaMime) << DV(runsEventLoop);

    d->putAllMimeToClipboard(runsEventLoop);
}

bool QOS2Clipboard::supportsMode(QClipboard::Mode mode) const
{
    return (mode == QClipboard::Clipboard);
}

bool QOS2Clipboard::ownsMode(QClipboard::Mode mode) const
{
    const bool result = mode == QClipboard::Clipboard && d ?
        data()->ownsClipboard() : false;

    qCInfo(lcQpaMime) << DV(mode) << DV(result);

    return result;
}

QOS2ClipboardData *QOS2Clipboard::data() const
{
    if (d == nullptr) {
        // Lazily create the data to avoid PM window creation prior to real use
        // (and before the event queue creation in which case it would fail).
        d = new QOS2ClipboardData(const_cast<QOS2Clipboard*>(this));
    }

    return reinterpret_cast<QOS2ClipboardData*>(d);
}

#endif
