/********************************************************************************
** Form generated from reading UI file 'dialog.ui'
**
** Created by: Qt User Interface Compiler version 5.12.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_DIALOG_H
#define UI_DIALOG_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QLabel>

QT_BEGIN_NAMESPACE

class Ui_Dialog
{
public:
    QLabel *label0;
    QLabel *label1;
    QLabel *label2;
    QLabel *label3;
    QLabel *label4;
    QLabel *label5;
    QLabel *label11;
    QLabel *label21;
    QLabel *label101;
    QLabel *label111;

    void setupUi(QDialog *Dialog)
    {
        if (Dialog->objectName().isEmpty())
            Dialog->setObjectName(QString::fromUtf8("Dialog"));
        Dialog->resize(400, 217);
        label0 = new QLabel(Dialog);
        label0->setObjectName(QString::fromUtf8("label0"));
        label0->setGeometry(QRect(10, 10, 381, 16));
        label0->setText(QString::fromUtf8("dummy"));
        label1 = new QLabel(Dialog);
        label1->setObjectName(QString::fromUtf8("label1"));
        label1->setGeometry(QRect(10, 30, 381, 16));
        label1->setText(QString::fromUtf8("dummy"));
        label2 = new QLabel(Dialog);
        label2->setObjectName(QString::fromUtf8("label2"));
        label2->setGeometry(QRect(10, 50, 381, 16));
        label2->setText(QString::fromUtf8("dummy"));
        label3 = new QLabel(Dialog);
        label3->setObjectName(QString::fromUtf8("label3"));
        label3->setGeometry(QRect(10, 70, 381, 16));
        label3->setText(QString::fromUtf8("dummy"));
        label4 = new QLabel(Dialog);
        label4->setObjectName(QString::fromUtf8("label4"));
        label4->setGeometry(QRect(10, 90, 381, 16));
        label4->setText(QString::fromUtf8("dummy"));
        label5 = new QLabel(Dialog);
        label5->setObjectName(QString::fromUtf8("label5"));
        label5->setGeometry(QRect(10, 110, 381, 16));
        label5->setText(QString::fromUtf8("dummy"));
        label11 = new QLabel(Dialog);
        label11->setObjectName(QString::fromUtf8("label11"));
        label11->setGeometry(QRect(10, 130, 381, 16));
        label11->setText(QString::fromUtf8("dummy"));
        label21 = new QLabel(Dialog);
        label21->setObjectName(QString::fromUtf8("label21"));
        label21->setGeometry(QRect(10, 150, 381, 16));
        label21->setText(QString::fromUtf8("dummy"));
        label101 = new QLabel(Dialog);
        label101->setObjectName(QString::fromUtf8("label101"));
        label101->setGeometry(QRect(10, 170, 381, 16));
        label101->setText(QString::fromUtf8("dummy"));
        label111 = new QLabel(Dialog);
        label111->setObjectName(QString::fromUtf8("label111"));
        label111->setGeometry(QRect(10, 190, 381, 16));
        label111->setText(QString::fromUtf8("dummy"));

        retranslateUi(Dialog);

        QMetaObject::connectSlotsByName(Dialog);
    } // setupUi

    void retranslateUi(QDialog *Dialog)
    {
        Dialog->setWindowTitle(QApplication::translate("Dialog", "Plural Sample", nullptr));
    } // retranslateUi

};

namespace Ui {
    class Dialog: public Ui_Dialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DIALOG_H
