/********************************************************************************
** Form generated from reading UI file 'mainwindow.ui'
**
** Created by: Qt User Interface Compiler version 5.12.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QAction *actionCalculate;
    QAction *actionExit;
    QAction *actionAbout;
    QWidget *centralWidget;
    QLabel *drivingLabel;
    QLabel *speedLabel;
    QLineEdit *distanceEdit;
    QLineEdit *speedEdit;
    QPushButton *calculateButton;
    QLabel *label;
    QLabel *label_2;
    QLabel *resultLabel;
    QMenuBar *menuBar;
    QMenu *menuFile;
    QMenu *menuHelp;
    QToolBar *mainToolBar;
    QStatusBar *statusBar;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
        MainWindow->resize(360, 154);
        actionCalculate = new QAction(MainWindow);
        actionCalculate->setObjectName(QString::fromUtf8("actionCalculate"));
        actionExit = new QAction(MainWindow);
        actionExit->setObjectName(QString::fromUtf8("actionExit"));
        actionAbout = new QAction(MainWindow);
        actionAbout->setObjectName(QString::fromUtf8("actionAbout"));
        centralWidget = new QWidget(MainWindow);
        centralWidget->setObjectName(QString::fromUtf8("centralWidget"));
        drivingLabel = new QLabel(centralWidget);
        drivingLabel->setObjectName(QString::fromUtf8("drivingLabel"));
        drivingLabel->setGeometry(QRect(10, 10, 111, 16));
        speedLabel = new QLabel(centralWidget);
        speedLabel->setObjectName(QString::fromUtf8("speedLabel"));
        speedLabel->setGeometry(QRect(130, 10, 111, 16));
        distanceEdit = new QLineEdit(centralWidget);
        distanceEdit->setObjectName(QString::fromUtf8("distanceEdit"));
        distanceEdit->setGeometry(QRect(10, 30, 113, 20));
        speedEdit = new QLineEdit(centralWidget);
        speedEdit->setObjectName(QString::fromUtf8("speedEdit"));
        speedEdit->setGeometry(QRect(130, 30, 113, 20));
        calculateButton = new QPushButton(centralWidget);
        calculateButton->setObjectName(QString::fromUtf8("calculateButton"));
        calculateButton->setGeometry(QRect(250, 28, 101, 23));
        label = new QLabel(centralWidget);
        label->setObjectName(QString::fromUtf8("label"));
        label->setGeometry(QRect(10, 60, 47, 41));
        label->setPixmap(QPixmap(QString::fromUtf8(":/Common/48x48/car_sedan_blue.png")));
        label_2 = new QLabel(centralWidget);
        label_2->setObjectName(QString::fromUtf8("label_2"));
        label_2->setGeometry(QRect(300, 60, 47, 41));
        label_2->setPixmap(QPixmap(QString::fromUtf8(":/Common/48x48/flag_united_kingdom.png")));
        resultLabel = new QLabel(centralWidget);
        resultLabel->setObjectName(QString::fromUtf8("resultLabel"));
        resultLabel->setGeometry(QRect(70, 60, 221, 41));
        MainWindow->setCentralWidget(centralWidget);
        menuBar = new QMenuBar(MainWindow);
        menuBar->setObjectName(QString::fromUtf8("menuBar"));
        menuBar->setGeometry(QRect(0, 0, 360, 21));
        menuFile = new QMenu(menuBar);
        menuFile->setObjectName(QString::fromUtf8("menuFile"));
        menuHelp = new QMenu(menuBar);
        menuHelp->setObjectName(QString::fromUtf8("menuHelp"));
        MainWindow->setMenuBar(menuBar);
        mainToolBar = new QToolBar(MainWindow);
        mainToolBar->setObjectName(QString::fromUtf8("mainToolBar"));
        MainWindow->addToolBar(Qt::TopToolBarArea, mainToolBar);
        statusBar = new QStatusBar(MainWindow);
        statusBar->setObjectName(QString::fromUtf8("statusBar"));
        MainWindow->setStatusBar(statusBar);

        menuBar->addAction(menuFile->menuAction());
        menuBar->addAction(menuHelp->menuAction());
        menuFile->addAction(actionCalculate);
        menuFile->addSeparator();
        menuFile->addAction(actionExit);
        menuHelp->addAction(actionAbout);

        retranslateUi(MainWindow);

        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "Driving Time", nullptr));
        actionCalculate->setText(QApplication::translate("MainWindow", "&Calculate", nullptr));
        actionExit->setText(QApplication::translate("MainWindow", "E&xit", nullptr));
        actionAbout->setText(QApplication::translate("MainWindow", "&About...", nullptr));
        drivingLabel->setText(QApplication::translate("MainWindow", "Driving distance:", nullptr));
        speedLabel->setText(QApplication::translate("MainWindow", "Speed:", nullptr));
        calculateButton->setText(QApplication::translate("MainWindow", "&Calculate", nullptr));
        label->setText(QString());
        label_2->setText(QString());
        resultLabel->setText(QString());
        menuFile->setTitle(QApplication::translate("MainWindow", "&File", nullptr));
        menuHelp->setTitle(QApplication::translate("MainWindow", "&Help", nullptr));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
