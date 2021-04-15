TARGET = mock5plugin
os2:TARGET_SHORT = mck5p

HEADERS += qmock5plugin.h
SOURCES += qmock5plugin.cpp
QT = mockplugins3

PLUGIN_TYPE = mockplugin
PLUGIN_CLASS_NAME = QMock5Plugin
PLUGIN_EXTENDS = -
load(qt_plugin)
