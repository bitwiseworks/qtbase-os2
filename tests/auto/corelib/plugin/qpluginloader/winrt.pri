# We cannot use TESTDATA as plugins have to reside physically
# inside the package directory
winrt|os2 {
    CONFIG(debug, debug|release) {
        DESTDIR = ../debug/bin
    } else {
        DESTDIR = ../release/bin
    }
}
