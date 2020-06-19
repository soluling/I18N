#include "dialog.h"
#include "ui_dialog.h"

Dialog::Dialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Dialog)
{
    ui->setupUi(this);

    setLabelValue(ui->label0, 0);
    setLabelValue(ui->label1, 1);
    setLabelValue(ui->label2, 2);
    setLabelValue(ui->label3, 3);
    setLabelValue(ui->label4, 4);
    setLabelValue(ui->label5, 5);
    setLabelValue(ui->label11, 11);
    setLabelValue(ui->label21, 21);
    setLabelValue(ui->label101, 101);
    setLabelValue(ui->label111, 111);
}

Dialog::~Dialog()
{
    delete ui;
}

void Dialog::setLabelValue(QLabel *label, int count)
{
    label->setText(tr("%n file", "", count));
}
