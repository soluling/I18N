using System;
using System.Windows;
using System.Threading;
using System.Globalization;

namespace FileDialog
{
  /// <summary>
  /// Interaction logic for App.xaml
  /// </summary>
  public partial class App : Application
  {
    protected override void OnStartup(StartupEventArgs e)
    {
      base.OnStartup(e);

      if (e.Args.Length > 0)
        Thread.CurrentThread.CurrentCulture = new CultureInfo(e.Args[0].ToString());
      else
        Thread.CurrentThread.CurrentCulture = CultureInfo.CurrentCulture;

      Thread.CurrentThread.CurrentUICulture = Thread.CurrentThread.CurrentCulture;
    }
  }
}
