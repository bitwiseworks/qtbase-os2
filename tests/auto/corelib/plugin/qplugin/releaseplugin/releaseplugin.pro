TEMPLATE = lib
CONFIG += plugin release
CONFIG -= debug debug_and_release
SOURCES = main.cpp
QT = core
DESTDIR = ../plugins
os2:TARGET_SHORT = releasep
