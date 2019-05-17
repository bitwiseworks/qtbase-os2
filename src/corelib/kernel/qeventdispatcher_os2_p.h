/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** Copyright (C) 2018 bww bitwise works GmbH. OS/2 parts.
**
** This file is part of the QtCore module of the Qt Toolkit.
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

#ifndef QEVENTDISPATCHER_OS2_P_H
#define QEVENTDISPATCHER_OS2_P_H

//
//  W A R N I N G
//  -------------
//
// This file is not part of the Qt API.  It exists purely as an
// implementation detail.  This header file may change from version to
// version without notice, or even be removed.
//
// We mean it.
//

#include "QtCore/qabstracteventdispatcher.h"
#include "QtCore/qt_os2.h"

QT_BEGIN_NAMESPACE

class QEventDispatcherOS2Private;

class Q_CORE_EXPORT QEventDispatcherOS2 : public QAbstractEventDispatcher
{
    Q_OBJECT
    Q_DECLARE_PRIVATE(QEventDispatcherOS2)

    friend class QGuiEventDispatcherPM;

public:
    explicit QEventDispatcherOS2(QObject *parent = 0);
    ~QEventDispatcherOS2();

    bool processEvents(QEventLoop::ProcessEventsFlags flags);
    bool hasPendingEvents();

    void registerSocketNotifier(QSocketNotifier *notifier);
    void unregisterSocketNotifier(QSocketNotifier *notifier);

    void registerTimer(int timerId, int interval, Qt::TimerType timerType, QObject *object);
    bool unregisterTimer(int timerId);
    bool unregisterTimers(QObject *object);
    QList<TimerInfo> registeredTimers(QObject *object) const;

    int remainingTime(int timerId);

    void wakeUp();
    void interrupt();
    void flush();

    void startingUp();
    void closingDown();
};

class Q_CORE_EXPORT QPMObjectWindow
{
public:
    QPMObjectWindow(bool deferred = false);
    virtual ~QPMObjectWindow();

    bool create();
    bool destroy();
    HWND hwnd() const { return w; }

    MRESULT send(ULONG msg, MPARAM mp1, MPARAM mp2) const {
        return WinSendMsg(w, msg, mp1, mp2);
    }

    bool post(ULONG msg, MPARAM mp1, MPARAM mp2) const {
        return WinPostMsg(w, msg, mp1, mp2);
    }

    virtual MRESULT message(ULONG msg, MPARAM mp1, MPARAM mp2) = 0;

private:
    static MRESULT EXPENTRY windowProc(HWND, ULONG, MPARAM, MPARAM);

    HWND w;
};

QT_END_NAMESPACE

#endif // QEVENTDISPATCHER_OS2_P_H
