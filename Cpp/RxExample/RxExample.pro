#-------------------------------------------------
#
# Project created by QtCreator 2019-06-17T10:36:18
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = RxExample
TEMPLATE = app

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

CONFIG += c++17

#INCLUDEPATH += /Users/zhaowei/Documents/Programs/OSS/RxCpp/Rx/v2/src \
#    /Users/zhaowei/Documents/Programs/OSS/rxqt/include

QMAKE_CXXFLAGS += /std:c++17 /permissive- /experimental:preprocessor

INCLUDEPATH += \
    C:\zw\vcpkg\installed\x86-windows\include\rxcpp \
    C:\zw\vcpkg\installed\x86-windows\include

SOURCES += \
        main.cpp \
        mainwindow.cpp

HEADERS += \
        mainwindow.h

FORMS += \
        mainwindow.ui

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target
