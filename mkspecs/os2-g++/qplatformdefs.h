/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** Copyright (C) 2010 netlabs.org. OS/2 parts.
** Copyright (C) 2018 bww bitwise works GmbH. OS/2 parts.
**
** This file is part of the qmake spec of the Qt Toolkit.
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

#ifndef QPLATFORMDEFS_H
#define QPLATFORMDEFS_H

// Drag in _osmajor, _abspath and others from <stdlib.h>
#ifndef _EMX_SOURCE
#define _EMX_SOURCE
#endif

// Get Qt defines/settings
#include "qglobal.h"

#include <io.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/process.h>
#include <stdlib.h>
#include <unistd.h>

// compiler-dependent network includes
#include <sys/socket.h>
#include <netinet/in.h>

#define Q_FS_FAT
#define QT_STATBUF              struct stat
#define QT_STATBUF4TSTAT        struct stat
#define QT_STAT                 ::stat
#define QT_FSTAT                ::fstat
#define QT_LSTAT                ::lstat
#define QT_STAT_REG             S_IFREG
#define QT_STAT_DIR             S_IFDIR
#define QT_STAT_MASK            S_IFMT
#if defined(_S_IFLNK)
#  define QT_STAT_LNK           S_IFLNK
#endif
#define QT_FILENO               fileno
#define QT_OPEN                 ::open
#define QT_TRUNCATE             ::truncate
#define QT_FTRUNCATE            ::ftruncate
#define QT_CLOSE                ::close
#define QT_LSEEK                ::lseek
#define QT_READ                 ::read
#define QT_WRITE                ::write
#define QT_ACCESS               ::access
#define QT_GETCWD               ::_getcwd2 // returns the drive letter with path
#define QT_CHDIR                ::_chdir2 // change the current drive if present
#define QT_MKDIR                ::mkdir
#define QT_RMDIR                ::rmdir
#define QT_OPEN_LARGEFILE       0
#define QT_OPEN_RDONLY          O_RDONLY
#define QT_OPEN_WRONLY          O_WRONLY
#define QT_OPEN_RDWR            O_RDWR
#define QT_OPEN_CREAT           O_CREAT
#define QT_OPEN_TRUNC           O_TRUNC
#define QT_OPEN_APPEND          O_APPEND
#define QT_OPEN_EXCL            O_EXCL
#if defined(O_TEXT)
# define QT_OPEN_TEXT           O_TEXT
# define QT_OPEN_BINARY         O_BINARY
#endif

#define QT_FOPEN                ::fopen
#define QT_FSEEK                ::fseeko
#define QT_FTELL                ::ftello
#define QT_FGETPOS              ::fgetpos
#define QT_FSETPOS              ::fsetpos
#define QT_FPOS_T               fpos_t
#define QT_OFF_T                off_t

#define QT_MMAP                 ::mmap

#define QT_DIR                  DIR
#define QT_DIRENT               struct dirent
#define QT_OPENDIR              ::opendir
#define QT_CLOSEDIR             ::closedir
#define QT_READDIR              ::readdir

#define QT_SIGNAL_ARGS          int

#define QT_VSNPRINTF            ::vsnprintf
#define QT_SNPRINTF             ::snprintf

#define QT_SOCKLEN_T            socklen_t

#define QT_SOCKET_CONNECT       ::connect
#define QT_SOCKET_BIND          ::bind

#endif // QPLATFORMDEFS_H
