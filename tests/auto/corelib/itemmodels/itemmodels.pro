TEMPLATE=subdirs

SUBDIRS = \
    qstringlistmodel \

qtHaveModule(gui): SUBDIRS += \
    qabstractitemmodel \
    qabstractproxymodel \
    qidentityproxymodel \
    qitemselectionmodel \
    qsortfilterproxymodel_recursive \

qtHaveModule(widgets) {
    SUBDIRS += \
        qsortfilterproxymodel

    qtHaveModule(sql): SUBDIRS += \
        qitemmodel
}
