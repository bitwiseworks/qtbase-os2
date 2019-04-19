/****************************************************************************
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

#ifndef QOS2CONTEXT_H
#define QOS2CONTEXT_H

#define OS2EMX_PLAIN_CHAR
#include <QtCore/qt_os2.h>

#include <QtCore/QLoggingCategory>
#include <QtCore/QRect>

QT_BEGIN_NAMESPACE

class QOS2Window;

Q_DECLARE_LOGGING_CATEGORY(lcQpaWindows)
Q_DECLARE_LOGGING_CATEGORY(lcQpaBackingStore)
Q_DECLARE_LOGGING_CATEGORY(lcQpaEvents)

#define DV(var) #var << var

namespace QOS2
{

inline QRect ToQRect(const RECTL &rcl, int parentHeight)
{
    // Flip y coordinate.
    return QRect(QPoint(rcl.xLeft, parentHeight - rcl.yTop),
                 QPoint(rcl.xRight - 1, parentHeight - (rcl.yBottom + 1)));
}

} // namespace QOS2Context

#ifndef QT_NO_DEBUG_STREAM
QDebug operator<<(QDebug d, const QOS2Window *window);
#endif

QT_END_NAMESPACE

#endif // QOS2CONTEXT_H
