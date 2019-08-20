TEMPLATE = subdirs
QT_FOR_CONFIG += gui-private

qtConfig(xkbcommon) {
    SUBDIRS += compose

    qtHaveModule(dbus) {
        !macos:!win32:!os2:SUBDIRS += ibus
    }
}

