#include "mainwindow.h"
#include <QApplication>
#include <rxqt.hpp>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    rxqt::run_loop rxqt_run_loop;
    w.show();

    return a.exec();
}
