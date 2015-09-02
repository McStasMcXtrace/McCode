
TEMPLATE = lib
CONFIG += staticlib
CONFIG -= qt

CONFIG(release, debug|release) {
    DESTDIR = Release
} else {
    DESTDIR = Debug
}

OBJECTS_DIR = $$DESTDIR

SOURCES += sgclib.c \
    sgfind.c \
    sghkl.c \
    sgio.c \
    sgsi.c \
    nxs.c
HEADERS += sginfo.h \
    nxs.h

INCLUDEPATH += ./


win32-msvc* { #ms-compiler
    QMAKE_CFLAGS += /D_CRT_SECURE_NO_WARNINGS
}

OTHER_FILES += \
    Doxyfile \
    nxs-files/*
