TEMPLATE = subdirs
QT_FOR_CONFIG += printsupport-private

osx:   SUBDIRS += cocoa
win32: SUBDIRS += windows
os2|unix:!darwin:qtConfig(cups): SUBDIRS += cups
