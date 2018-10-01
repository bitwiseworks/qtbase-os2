CONFIG += testcase
TARGET = tst_qfilesystementry
QT = core-private testlib
DEFINES = QT_BUILD_CORE_LIB # to avoid Q_DECL_IMPORT on QFileSystemEntry::isRoot
SOURCES = tst_qfilesystementry.cpp \
    $$QT_SOURCE_TREE/src/corelib/io/qfilesystementry.cpp
HEADERS = $$QT_SOURCE_TREE/src/corelib/io/qfilesystementry_p.h
