qtConfig(printpreviewwidget) {
    HEADERS += widgets/qprintpreviewwidget.h
    SOURCES += widgets/qprintpreviewwidget.cpp
}

unix|os2:!darwin:qtConfig(cupsjobwidget) {
    HEADERS += widgets/qcupsjobwidget_p.h
    SOURCES += widgets/qcupsjobwidget.cpp
    FORMS += widgets/qcupsjobwidget.ui

}

INCLUDEPATH += $$PWD
