win32 {
   SOURCES = main_win.cpp
   LIBS += -luser32
}
os2 {
   SOURCES = main_os2.cpp
}
unix {
   SOURCES = main_unix.cpp
}

CONFIG -= qt
CONFIG += cmdline
DESTDIR = ./
QT = core
