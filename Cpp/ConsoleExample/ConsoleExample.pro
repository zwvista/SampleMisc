#-------------------------------------------------
#
# Project created by QtCreator 2019-06-17T10:36:18
#
#-------------------------------------------------

QT -= gui

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

CONFIG += c++17 console
CONFIG -= app_bundle

#INCLUDEPATH += /Users/zhaowei/Documents/Programs/OSS/RxCpp/Rx/v2/src \
#    /Users/zhaowei/Documents/Programs/OSS/rxqt/include

QMAKE_CXXFLAGS += /std:c++17 /permissive-

INCLUDEPATH += C:\zw\vcpkg\installed\x86-windows\include

LIBS += -LC:\zw\vcpkg\installed\x86-windows\debug\lib \
    -lcpprest_2_10d

SOURCES += \
        ConsoleExample\main.cpp \
#        ConsoleExample\coroutine.cpp \
        ConsoleExample\Rest\httpclient1.cpp \
#        ConsoleExample\Rest\json1.cpp \
#        ConsoleExample\Rest\json2.cpp \
#        ConsoleExample\Rest\json3.cpp \
#        ConsoleExample\Rest\Post.cpp \
#        ConsoleExample\Rest\rx1.cpp \
#        ConsoleExample\Range\jys.cpp \

#HEADERS  += ConsoleExample\Rest\Post.hpp \
#        ConsoleExample\Rest\RestApi.hpp \

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target
