TARGET = mock6plugin
os2:TARGET_SHORT = mck6p

HEADERS += qmock6plugin.h
SOURCES += qmock6plugin.cpp
QT = mockplugins3

PLUGIN_TYPE = mockauxplugin
PLUGIN_CLASS_NAME = QMock6Plugin
load(qt_plugin)
