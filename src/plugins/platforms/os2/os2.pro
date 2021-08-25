TARGET = qos2

QT += \
    core-private gui-private \
    eventdispatcher_support-private fontdatabase_support-private

SOURCES = \
    main.cpp \
    qos2backingstore.cpp \
    qos2clipboard.cpp \
    qos2context.cpp \
    qos2cursor.cpp \
    qos2integration.cpp \
    qos2keymapper.cpp \
    qos2mime.cpp \
    qos2screen.cpp \
    qos2theme.cpp \
    qos2window.cpp

HEADERS = \
    qos2backingstore.h \
    qos2clipboard.h \
    qos2context.h \
    qos2cursor.h \
    qos2integration.h \
    qos2keymapper.h \
    qos2mime.h \
    qos2screen.h \
    qos2theme.h \
    qos2window.h

OTHER_FILES += os2.json

PLUGIN_TYPE = platforms
PLUGIN_CLASS_NAME = QOS2IntegrationPlugin
!equals(TARGET, $$QT_DEFAULT_QPA_PLUGIN): PLUGIN_EXTENDS = -
load(qt_plugin)
