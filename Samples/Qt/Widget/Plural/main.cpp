#include <QApplication>
#include <QLocale>
#include <QTranslator>
#include "dialog.h"

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);

  // Create translator object and load translations
  QTranslator translator;
  bool ok = translator.load(":/i18n/strings_" + QLocale::system().name());

  if (ok)
    a.installTranslator(&translator);

  Dialog w;
  w.show();

  return a.exec();
}
