# History of changes for Qt 5 Base for OS/2

#### Version 5.15.2 OS/2 Beta 1 (2021-09-22)

* Update source code to version 5.15.2.
* configure: Disable GLib usage on OS/2.
* mkspecs/os2-g++: Disable AVC until GCC alignment is fixed.
* mkspecs/os2-g++: Don't force debug_and_release any more.
* qmake: Disable relocatable qmake binary on OS/2 [#132].
* cmake: Optimize TARGET_SHORT processing on OS/2.
* cmake: Avoid OpenGL tests when no opengl module is available.
* corelib: Avoid waking up event queue by DeferredDelete events on OS/2 [#121].
* corelib: Fix build breaks on platforms where st_atimespec is a macro.
* corelib: Fix generating internal MIME database on OS/2 [#128].
* corelib: Get rid of timer handling assertion in QEventDispatcherOS2 [#70].
* network: Use socklen_t for QT_SOCKLEN_T on OS/2.
* gui: Fix QDesktopServices compatibility code on OS/2 [#70].
* qml: Prefix TARGET_SHORT with d similar to declarative_ for TARGET.
* platforms/os2: Implement QCursor::pos and QCursor::setPos machinery [#127].
* platforms/os2: Do not raise and activate platform window upon creation [#126].
* platforms/os2: Support changing geometry of minimized windows [#126].
* platforms/os2: Fix tst_QWindow glitches [#126].
* platforms/os2: Fix assertion in QOS2Screen preventing reinitialization [#70].
* platforms/os2: Recognize Qt windows when changing focus [#70].
* platforms/os2: Use "monospace" as the default fixed font [#70].
* platforms/os2: Add system clipboard integration [#123].

#### Version 5.13.1 OS/2 Beta 1 (2019-10-17)

* Update source code to version 5.13.1.
* qmake: Port resolving of library options to OS/2 [#101].
* qmake: Use import libraries for Qt module DLL resolving on OS/2 [#101].
* qmake: Fix extra CR in target dependency lists on OS/2 [#101].
* qmake: Support creating long symlinks to TARGET_SHORT DLLs on OS/2 [#105].
* cmake: Fix DLL names on OS/2 [#102].
* cmake: Avoid linking to qtmain.lib on OS/2 [#108].
* tools/rcc: Disable unneeded LF->CRLF translation on OS/2 [#106].
* corelib: Fix rounding to millisecond in QTimer [#110].
* corelib/os2: Uninit QEventDispatcher's message queue as late as possible [qtdeclarative-os2#1].
* corelib/os2: Fix QDir::absolteFirPath for path rewrites [#114].
* corelib/os2: Fix QFileInfo stat for lower case drive roots [#106].
* gui/os2: Fix default size for top-level windows with system frame [#112].
* network/os2: Fix issuer certificate checking [#96].
* platforms/os2: Align to 5.13 QPA API changes [#101].
* platforms/os2: Don't let titlebar disappear for default window position [qttools-os2#1].
* platforms/os2: Fix garbage in translucent top-level widgets [#109].
* platforms/os2: Create transient QWindow parent's platform window as needed [qtdeclarative-os2#1].
* plugins: Enable CUPS print support plugin on OS/2 [#111].
* examples/network/torrent: Fix numerous bugs leading to crashes [#100].

#### Version 5.11.0 OS/2 Beta 1 (2019-08-12)

* First public release.
