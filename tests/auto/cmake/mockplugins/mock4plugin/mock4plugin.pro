TARGET = mock4plugin
os2:TARGET_SHORT = mck4p

HEADERS += qmock4plugin.h
SOURCES += qmock4plugin.cpp
QT = mockplugins1

PLUGIN_TYPE = mockplugin
PLUGIN_CLASS_NAME = QMock4Plugin
PLUGIN_EXTENDS = -
load(qt_plugin)
