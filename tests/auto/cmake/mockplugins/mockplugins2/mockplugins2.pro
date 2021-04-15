TARGET = QtMockPlugins2
os2:TARGET_SHORT = QtMckP2
QT = core
SOURCES += fake.cpp # Needed to make libtool / ar happy on macOS
load(qt_module)
