TEMPLATE = lib
CONFIG += plugin debug
CONFIG -= release debug_and_release
SOURCES = main.cpp
QT = core
DESTDIR = ../plugins
os2:TARGET_SHORT = debugp
