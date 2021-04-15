TARGET = mock1plugin

HEADERS += qmock1plugin.h
SOURCES += qmock1plugin.cpp
QT = mockplugins1

PLUGIN_TYPE = mockplugin
PLUGIN_CLASS_NAME = QMock1Plugin
os2:TARGET_SHORT = mck1p
load(qt_plugin)
