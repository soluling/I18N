#include "dialog.h"
#include "ui_dialog.h"

Dialog::Dialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Dialog)
{
    ui->setupUi(this);

    ui->label2->setText(tr("This is a sample"));
}

Dialog::~Dialog()
{
    delete ui;
}
