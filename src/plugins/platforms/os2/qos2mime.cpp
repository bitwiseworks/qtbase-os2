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

#include "qos2context.h"

#include "qos2mime.h"

#include <QtGui/private/qinternalmimedata_p.h>

QT_BEGIN_NAMESPACE

class QOS2MimeList
{
public:
    QOS2MimeList();
    ~QOS2MimeList();
    void addMime(QOS2Mime *mime);
    void removeMime(QOS2Mime *mime);
    QList<QOS2Mime*> mimes();

private:
    void init();
    bool initialized;
    QList<QOS2Mime*> list;
};

Q_GLOBAL_STATIC(QOS2MimeList, theMimeList);

/*!
    \class QOS2Mime
    \brief The QOS2Mime class maps open-standard MIME to OS/2 PM Clipboard
    formats.
    \ingroup io
    \ingroup draganddrop
    \ingroup misc

    Qt's drag-and-drop and clipboard facilities use the MIME standard.
    On X11, this maps trivially to the Xdnd protocol, but on OS/2
    although some applications use MIME types to describe clipboard
    formats, others use arbitrary non-standardized naming conventions,
    or unnamed built-in formats of the Presentation Manager.

    By instantiating subclasses of QOS2Mime that provide conversions between OS/2
    PM Clipboard and MIME formats, you can convert proprietary clipboard formats
    to MIME formats.

    Qt has predefined support for the following PM Clipboard formats (custom
    formats registered in the system atom table by name are given in double
    quotes):

    \table
    \header \o PM Format \o Equivalent MIME type
    \row \o \c CF_TEXT          \o \c text/plain (system codepage,
                                   zero-terminated string)
    \row \o \c "text/unicode"   \o \c text/plain (16-bit Unicode,
                                   zero-terminated string, Mozilla-compatible)
    \row \o \c "text/html"      \o \c text/html (16-bit Unicode,
                                   zero-terminated string, Mozilla-compatible)
    \row \o \c CF_BITMAP        \o \c{image/xyz}, where \c xyz is
                                   a \l{QImageWriter::supportedImageFormats()}{Qt image format}
    \row \o \c "x-mime:<mime>"  \o data in the format corresponding to the given
                                   MIME type \c <mime>
    \endtable

    Note that all "x-mime:<mime>" formats use the \c CFI_POINTER storage type.
    That is, the clipboard contains a pointer to the memory block containing the
    MIME data in the corresponding format. The first 4 bytes of this memory
    block always contain the length of the subsequent MIME data array, in bytes.

    An example use of this class by the user application would be to map the
    PM Metafile clipboard format (\c CF_METAFILE) to and from the MIME type
    \c{image/x-metafile}. This conversion might simply be adding or removing a
    header, or even just passing on the data. See \l{Drag and Drop} for more
    information on choosing and definition MIME types.
*/

/*!
Constructs a new conversion object, adding it to the globally accessed
list of available converters.
*/
QOS2Mime::QOS2Mime()
{
    theMimeList()->addMime(this);
}

/*!
Destroys a conversion object, removing it from the global
list of available converters.
*/
QOS2Mime::~QOS2Mime()
{
    theMimeList()->removeMime(this);
}

/*!
    Registers the MIME type \a mime, and returns an ID number
    identifying the format on OS/2. Intended to be used by QOS2Mime
    implementations for registering custom clipboard formats they use.
*/
// static
ULONG QOS2Mime::registerMimeType(const QString &mime)
{
    ULONG cf = WinAddAtom(WinQuerySystemAtomTable(), mime.toLocal8Bit());
    if (!cf) {
        qCWarning(lcQpaMime) << "WinAddAtom failed with" << Qt::hex << WinGetLastError(NULLHANDLE);
        return 0;
    }

    return cf;
}

/*!
    Unregisters the MIME type identified by \a mimeId which was previously
    registered with registerMimeType().
*/
// static
void QOS2Mime::unregisterMimeType(ULONG mimeId)
{
    WinDeleteAtom(WinQuerySystemAtomTable(), mimeId);
}

/*!
    Returns a list of all currently defined QOS2Mime objects.
*/
// static
QList<QOS2Mime*> QOS2Mime::all()
{
    return theMimeList()->mimes();
}

/*!
    Allocates a block of shared memory of the given \a size and returns the
    address of this block. This memory block may be then filled with data and
    returned by convertFromMimeData() as the value of the \c CFI_POINTER type.
*/
// static
ULONG QOS2Mime::allocateMemory(size_t size)
{
    if (size == 0)
        return 0;

    ULONG data = 0;

    // allocate giveable memory for the array
    APIRET arc = DosAllocSharedMem((PVOID *)&data, NULL, size,
                                   PAG_WRITE  | PAG_COMMIT | OBJ_GIVEABLE);
    if (arc != NO_ERROR) {
        qCWarning(lcQpaMime) << "DosAllocSharedMem failed with" << arc;
        return 0;
    }

    return data;
}

/*!
    Frees a memory block \a addr allocated by allocateMemory(). Normally, not
    used because the \c CFI_POINTER memory blocks are owned by the system after
    convertFromMimeData() returns.
*/
// static
void QOS2Mime::freeMemory(ULONG addr)
{
    DosFreeMem((PVOID)addr);
}

/*!
    \fn QList<MimeCFPair> QOS2Mime::formatsForMimeData(const QMimeData *mimeData) const

    Returns a list of ULONG values representing the different OS/2 PM
    clipboard formats that can be provided for the \a mimeData, in order of
    precedence (the most suitable format goes first), or an empty list if
    neither of the mime types provided by \a mimeData is supported by this
    converter. Note that each item in the returned list is actually a pair
    consisting of the mime type name and the corresponding format identifier.

    All subclasses must reimplement this pure virtual function.
*/

/*!
    \fn bool QOS2Mime::convertFromMimeData(const QMimeData *mimeData, ULONG format,
                                           ULONG &flags, ULONG *data) const

    Converts the \a mimeData to the specified \a format.

    If \a data is not NULL, a handle to the converted data should be then placed
    in a variable pointed to by \a data and with the necessary flags describing
    the handle returned in the \a flags variable.

    The following flags describing the data storage type are recognized:

    \table
    \row \o \c CFI_POINTER        \o \a data is a pointer to a block of memory
                                      allocated with QOS2Mime::allocateMemory()
    \row \o \c CFI_HANDLE         \o \a data is a handle to the appropriate
                                      PM resource
    \endtable

    If \a data is NULL then a delayed conversion is requested by the caller.
    The implementation should return the appropriate flags in the \a flags
    variable and may perform the real data conversion later when this method is
    called again with \a data being non-NULL.

    Return true if the conversion was successful.

    All subclasses must reimplement this pure virtual function.
*/

/*!
    \fn QList<MimeCFPair> QOS2Mime::mimesForFormats(const QList<ULONG> &formats) const

    Returns a list of mime types that will be created form the specified list of
    \a formats, in order of precedence (the most suitable mime type comes
    first), or an empty list if neither of the \a formats is supported by this
    converter. Note that each item in the returned list is actually a pair
    consisting of the mime type name and the corresponding format identifier.

    All subclasses must reimplement this pure virtual function.
*/

/*!
    \fn QVariant QOS2Mime::convertFromFormat(ULONG format, ULONG flags, ULONG data,
                                             const QString &mimeType,
                                             QVariant::Type preferredType) const

    Returns a QVariant containing the converted from the \a data in the
    specified \a format with the given \a flags to the requested \a mimeType. If
    possible the QVariant should be of the \a preferredType to avoid needless
    conversions.

    All subclasses must reimplement this pure virtual function.
*/

// static
QList<QOS2Mime::Match> QOS2Mime::allConvertersFromFormats(const QList<ULONG> &formats)
{
    QList<Match> matches;

    QList<QOS2Mime*> mimes = theMimeList()->mimes();
    foreach(QOS2Mime *mime, mimes) {
        QList<MimeCFPair> fmts = mime->mimesForFormats(formats);
        int priority = 0;
        foreach (MimeCFPair fmt, fmts) {
            ++priority;
            QList<Match>::iterator it = matches.begin();
            for (; it != matches.end(); ++it) {
                Match &match = *it;
                if (match.mime == fmt.mime) {
                    // replace if priority is higher, ignore otherwise
                    if (priority < match.priority) {
                        match.converter = mime;
                        match.format = fmt.format;
                        match.priority = priority;
                    }
                    break;
                }
            }
            if (it == matches.end()) {
                matches += Match(mime, fmt.mime, fmt.format, priority);
            }
        }
    }

    return matches;
}

// static
QList<QOS2Mime::Match> QOS2Mime::allConvertersFromMimeData(const QMimeData *mimeData)
{
    QList<Match> matches;

    QList<QOS2Mime*> mimes = theMimeList()->mimes();
    foreach(QOS2Mime *mime, mimes) {
        QList<MimeCFPair> fmts = mime->formatsForMimeData(mimeData);
        int priority = 0;
        foreach (MimeCFPair fmt, fmts) {
            ++priority;
            QList<Match>::iterator it = matches.begin();
            for (; it != matches.end(); ++it) {
                Match &match = *it;
                if (mime == mimes.last()) { // QOS2MimeAnyMime?
                    if (match.mime == fmt.mime){
                        // we assume that specialized converters (that come
                        // first) provide a more precise conversion than
                        // QOS2MimeAnyMime and don't let it get into the list in
                        // order to avoid unnecessary duplicate representations
                        break;
                    }
                }
                if (match.format == fmt.format) {
                    // replace if priority is higher, ignore otherwise
                    if (priority < match.priority) {
                        match.converter = mime;
                        match.mime = fmt.mime;
                        match.priority = priority;
                    }
                    break;
                }
            }
            if (it == matches.end()) {
                matches += Match(mime, fmt.mime, fmt.format, priority);
            }
        }
    }

    return matches;
}


/*!
    Returns a string representation of the given clipboard \a format. The
    string representation is obtained by querying the system atom table.
*/
// static
QString QOS2Mime::formatName(ULONG format)
{
    QString name;
    HATOMTBL tbl = WinQuerySystemAtomTable();
    if (tbl != NULLHANDLE) {
        ULONG len = WinQueryAtomLength(tbl, format);
        QByteArray atom(len, '\0');
        WinQueryAtomName(tbl, format, atom.data(), atom.size() + 1);
        name = QString::fromLocal8Bit(atom);
    }
    return name;
}

class QOS2MimeText : public QOS2Mime
{
public:
    QOS2MimeText();
    ~QOS2MimeText();

    // for converting from Qt
    QList<MimeCFPair> formatsForMimeData(const QMimeData *mimeData) const;
    bool convertFromMimeData(const QMimeData *mimeData, ULONG format,
                             ULONG &flags, ULONG *data) const;

    // for converting to Qt
    QList<MimeCFPair> mimesForFormats(const QList<ULONG> &formats) const;
    QVariant convertFromFormat(ULONG format, ULONG flags, ULONG data,
                               const QString &mimeType,
                               QVariant::Type preferredType) const;

    const ULONG CF_TextUnicode;
    const ULONG CF_TextHtml;
};

QOS2MimeText::QOS2MimeText()
    // "text/unicode" is what Mozilla uses to for unicode
    : CF_TextUnicode (registerMimeType(QLatin1String("text/unicode")))
    // "text/html" is what Mozilla uses to for HTML
    , CF_TextHtml (registerMimeType(QLatin1String("text/html")))
{
}

QOS2MimeText::~QOS2MimeText()
{
    unregisterMimeType(CF_TextHtml);
    unregisterMimeType(CF_TextUnicode);
}

QList<QOS2Mime::MimeCFPair> QOS2MimeText::formatsForMimeData(const QMimeData *mimeData) const
{
    QList<MimeCFPair> fmts;
    // prefer HTML as it's reacher
    if (mimeData->hasHtml())
        fmts << MimeCFPair(QLatin1String("text/html"), CF_TextHtml);
    // prefer unicode over local8Bit
    if (mimeData->hasText())
        fmts << MimeCFPair(QLatin1String("text/plain"), CF_TextUnicode)
             << MimeCFPair(QLatin1String("text/plain"), CF_TEXT);
    return fmts;
}

// text/plain is defined as using CRLF, but so many programs don't,
// and programmers just look for '\n' in strings.
// OS/2 really needs CRLF, so we ensure it here.
bool QOS2MimeText::convertFromMimeData(const QMimeData *mimeData, ULONG format,
                                      ULONG &flags, ULONG *data) const
{
    if (!mimeData->hasText() ||
        (format != CF_TEXT && format != CF_TextUnicode && format != CF_TextHtml))
        return false;

    flags = CFI_POINTER;

    if (data == NULL)
        return true; // delayed rendering, nothing to do

    QByteArray r;

    if (format == CF_TEXT) {
        QByteArray str = mimeData->text().toLocal8Bit();
        // Anticipate required space for CRLFs at 1/40
        int maxsize = str.size()+str.size()/40+1;
        r.fill('\0', maxsize);
        char *o = r.data();
        const char *d = str.data();
        const int s = str.size();
        bool cr = false;
        int j = 0;
        for (int i = 0; i < s; i++) {
            char c = d[i];
            if (c == '\r')
                cr = true;
            else {
                if (c == '\n') {
                    if (!cr)
                        o[j++] = '\r';
                }
                cr = false;
            }
            o[j++] = c;
            if (j+1 >= maxsize) {
                maxsize += maxsize/4;
                r.resize(maxsize);
                o = r.data();
            }
        }
        if (j < r.size())
            o[j] = '\0';
    } else if (format == CF_TextUnicode || CF_TextHtml) {
        QString str = format == CF_TextUnicode ?
                      mimeData->text() : mimeData->html();
        const QChar *u = str.unicode();
        QString res;
        const int s = str.length();
        int maxsize = s + s/40 + 3;
        res.resize(maxsize);
        int ri = 0;
        bool cr = false;
        for (int i = 0; i < s; ++i) {
            if (*u == QLatin1Char('\r'))
                cr = true;
            else {
                if (*u == QLatin1Char('\n') && !cr)
                    res[ri++] = QLatin1Char('\r');
                cr = false;
            }
            res[ri++] = *u;
            if (ri+3 >= maxsize) {
                maxsize += maxsize/4;
                res.resize(maxsize);
            }
            ++u;
        }
        res.truncate(ri);
        const int byteLength = res.length()*2;
        r.fill('\0', byteLength + 2);
        memcpy(r.data(), res.unicode(), byteLength);
        r[byteLength] = 0;
        r[byteLength+1] = 0;
    } else{
        return false;
    }

    *data = QOS2Mime::allocateMemory(r.size());
    if (!*data)
        return false;

    memcpy((void *)*data, r.data(), r.size());
    return true;
}

QList<QOS2Mime::MimeCFPair> QOS2MimeText::mimesForFormats(const QList<ULONG> &formats) const
{
    QList<MimeCFPair> mimes;
    // prefer HTML as it's reacher
    if (formats.contains(CF_TextHtml))
        mimes << MimeCFPair(QLatin1String("text/html"), CF_TextHtml);
    // prefer unicode over local8Bit
    if (formats.contains(CF_TextUnicode))
        mimes << MimeCFPair(QLatin1String("text/plain"), CF_TextUnicode);
    if (formats.contains(CF_TEXT))
        mimes << MimeCFPair(QLatin1String("text/plain"), CF_TEXT);
    return mimes;
}

QVariant QOS2MimeText::convertFromFormat(ULONG format, ULONG flags, ULONG data,
                                         const QString &mimeType,
                                         QVariant::Type preferredType) const
{
    QVariant ret;

    if (!mimeType.startsWith(QLatin1String("text/plain")) &&
        !mimeType.startsWith(QLatin1String("text/html")))
        return ret;
    if ((format != CF_TEXT && format != CF_TextUnicode && format != CF_TextHtml) ||
        !(flags & CFI_POINTER) || !data)
        return ret;

    QString str;

    if (format == CF_TEXT) {
        const char *d = (const char *)data;
        QByteArray r("");
        if (*d) {
            const int s = qstrlen(d);
            r.fill('\0', s);
            char *o = r.data();
            int j = 0;
            for (int i = 0; i < s; i++) {
                char c = d[i];
                if (c != '\r')
                    o[j++] = c;
            }
        }
        str = QString::fromLocal8Bit(r);
    } else if (format == CF_TextUnicode || CF_TextHtml) {
        str = QString::fromUtf16((const unsigned short *)data);
        str.replace(QLatin1String("\r\n"), QLatin1String("\n"));
    }

    if (preferredType == QVariant::String)
        ret = str;
    else
        ret = str.toUtf8();

    return ret;
}

class QOS2MimeAnyMime : public QOS2Mime
{
public:
    QOS2MimeAnyMime();
    ~QOS2MimeAnyMime();

    // for converting from Qt
    QList<MimeCFPair> formatsForMimeData(const QMimeData *mimeData) const;
    bool convertFromMimeData(const QMimeData *mimeData, ULONG format,
                             ULONG &flags, ULONG *data) const;
    // for converting to Qt
    QList<MimeCFPair> mimesForFormats(const QList<ULONG> &formats) const;
    QVariant convertFromFormat(ULONG format, ULONG flags, ULONG data,
                               const QString &mimeType,
                               QVariant::Type preferredType) const;

private:
    ULONG registerMimeType(const QString &mime) const;
    QString registerFormat(ULONG format) const;

    mutable QMap<QString, ULONG> cfMap;
    mutable QMap<ULONG, QString> mimeMap;

    static QStringList ianaTypes;
    static QString mimePrefix;
    static QString customPrefix;
};

// static
QStringList QOS2MimeAnyMime::ianaTypes;
QString QOS2MimeAnyMime::mimePrefix;
QString QOS2MimeAnyMime::customPrefix;

QOS2MimeAnyMime::QOS2MimeAnyMime()
{
    //MIME Media-Types
    if (!ianaTypes.size()) {
        ianaTypes.append(QLatin1String("application/"));
        ianaTypes.append(QLatin1String("audio/"));
        ianaTypes.append(QLatin1String("example/"));
        ianaTypes.append(QLatin1String("image/"));
        ianaTypes.append(QLatin1String("message/"));
        ianaTypes.append(QLatin1String("model/"));
        ianaTypes.append(QLatin1String("multipart/"));
        ianaTypes.append(QLatin1String("text/"));
        ianaTypes.append(QLatin1String("video/"));

        mimePrefix = QLatin1String("x-mime:");
        customPrefix = QLatin1String("application/x-qt-pm-mime;value=\"");
    }
}

QOS2MimeAnyMime::~QOS2MimeAnyMime()
{
    foreach(ULONG cf, cfMap.values())
        unregisterMimeType(cf);
}

QList<QOS2Mime::MimeCFPair> QOS2MimeAnyMime::formatsForMimeData(const QMimeData *mimeData) const
{
    QList<MimeCFPair> fmts;

    QStringList mimes = QInternalMimeData::formatsHelper(mimeData);
    foreach (QString mime, mimes) {
        ULONG cf = cfMap.value(mime);
        if (!cf)
            cf = registerMimeType(mime);
        if (cf)
            fmts << MimeCFPair(mime, cf);
    }

    return fmts;
}

bool QOS2MimeAnyMime::convertFromMimeData(const QMimeData *mimeData, ULONG format,
                                          ULONG &flags, ULONG *data) const
{
    QString mime = mimeMap.value(format);
    if (mime.isNull())
        return false;

    flags = CFI_POINTER;

    if (data == NULL)
        return true; // delayed rendering, nothing to do

    QByteArray r = QInternalMimeData::renderDataHelper(mime, mimeData);
    if (r.isNull())
        return false;

    *data = QOS2Mime::allocateMemory(r.size() + sizeof(ULONG));
    if (!*data)
        return false;

    *((ULONG *)(*data)) = r.size();
    memcpy((void *)(*data + sizeof(ULONG)), r.data(), r.size());
    return true;
}

QList<QOS2Mime::MimeCFPair> QOS2MimeAnyMime::mimesForFormats(const QList<ULONG> &formats) const
{
    QList<MimeCFPair> mimes;

    foreach (ULONG format, formats) {
        QString mime = mimeMap.value(format);
        if (mime.isEmpty())
            mime = registerFormat(format);
        if (!mime.isEmpty())
            mimes << MimeCFPair(mime, format);
    }

    return mimes;
}

QVariant QOS2MimeAnyMime::convertFromFormat(ULONG format, ULONG flags, ULONG data,
                                            const QString &mimeType,
                                            QVariant::Type preferredType) const
{
    Q_UNUSED(preferredType);

    QVariant ret;

    if (cfMap.value(mimeType) != format)
        return ret;

    if (!(flags & CFI_POINTER) || !data)
        return ret;

    // get the real block size (always rounded to the page boundary (4K))
    ULONG sz = ~0, fl = 0, arc;
    arc = DosQueryMem((PVOID)data, &sz, &fl);
    if (arc != NO_ERROR) {
        qCWarning(lcQpaMime) << "DosQueryMem failed with" << arc;
        return ret;
    }
    ULONG size = *((ULONG *)data);
    if (!size || size + sizeof(ULONG) > sz)
        return ret;

    // it should be enough to return the data and let QMimeData do the rest.
    ret = QByteArray((const char *)(data + sizeof(ULONG)), size);
    return ret;
}

ULONG QOS2MimeAnyMime::registerMimeType(const QString &mime) const
{
    if (mime.isEmpty())
        return 0;

    QString mimeToReg = mime;

    bool ianaType = false;
    foreach(QString prefix, ianaTypes) {
        if (mime.startsWith(prefix)) {
            ianaType = true;
            break;
        }
    }
    if (!ianaType) {
        // prepend the non-standard type with the prefix that makes it comply
        // with the standard
        mimeToReg = customPrefix + mime + QLatin1Char('\"');
    }

    mimeToReg = mimePrefix + mimeToReg;
    ULONG cf = QOS2Mime::registerMimeType(mimeToReg);
    if (cf) {
        cfMap[mime] = cf;
        mimeMap[cf] = mime;
    }
    return cf;
}

QString QOS2MimeAnyMime::registerFormat(ULONG format) const
{
    QString mime;

    if (!format)
        return mime;

    QString atomStr = formatName(format);
    if (atomStr.startsWith(mimePrefix)) {
        // the format represents the mime type we can recognize
        // increase the reference count
        ULONG cf = QOS2Mime::registerMimeType(atomStr);
        Q_ASSERT(cf == format);
        // extract the real mime type (w/o our prefix)
        mime = atomStr.mid(mimePrefix.size());
        if (!mime.isEmpty()) {
            cfMap[mime] = cf;
            mimeMap[cf] = mime;
        }
    }
    return mime;
}

QOS2MimeList::QOS2MimeList()
    : initialized(false)
{
}

QOS2MimeList::~QOS2MimeList()
{
    while (list.size())
        delete list.first();
}


void QOS2MimeList::init()
{
    if (!initialized) {
        initialized = true;
        new QOS2MimeAnyMime; // must be the first (used as a fallback)
        new QOS2MimeText;
    }
}

void QOS2MimeList::addMime(QOS2Mime *mime)
{
    init();
    list.prepend(mime);
}

void QOS2MimeList::removeMime(QOS2Mime *mime)
{
    init();
    list.removeAll(mime);
}

QList<QOS2Mime*> QOS2MimeList::mimes()
{
    init();
    return list;
}

QT_END_NAMESPACE
