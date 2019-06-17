#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <rxqt.hpp>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

//    auto f = [](const QLineEdit* edt) {
//        return rxqt::from_signal(edt, &QLineEdit::textChanged)
//            .start_with(edt->text());
//    };
//    f(ui->edtNumber1).combine_latest([](const QString& s1, const QString& s2, const QString& s3){
//        return s1.toInt() + s2.toInt() + s3.toInt();
//    }, f(ui->edtNumber2), f(ui->edtNumber3))
//    .subscribe([&](int n){ui->lblResult->setNum(n);});

    add();
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::on_edtNumber1_textChanged(const QString &arg1)
{
    add();
}

void MainWindow::on_edtNumber2_textChanged(const QString &arg1)
{
    add();
}

void MainWindow::on_edtNumber3_textChanged(const QString &arg1)
{
    add();
}

void MainWindow::add()
{
    int n = ui->edtNumber1->text().toInt() + ui->edtNumber2->text().toInt() + ui->edtNumber3->text().toInt();
    ui->lblResult->setNum(n);
}
