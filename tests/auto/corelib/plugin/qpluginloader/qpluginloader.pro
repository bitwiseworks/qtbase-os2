QT = core
TEMPLATE = subdirs

tst.depends = lib theplugin
SUBDIRS = lib \
          theplugin \
          tst
!android:!win32:!darwin:!os2 {
    tst.depends += almostplugin
    SUBDIRS += almostplugin
}
macos:qtConfig(private_tests) {
    tst.depends += machtest
    SUBDIRS += machtest
}

# no special install rule for subdir
INSTALLS =


