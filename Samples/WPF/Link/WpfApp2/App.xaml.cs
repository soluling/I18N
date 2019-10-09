using System.Windows;
using System.Threading;
using System.Globalization;

namespace WpfApp2
{
  /// <summary>
  /// Interaction logic for App.xaml
  /// </summary>
  public partial class App : Application
  {
    public App()
    {
      Thread.CurrentThread.CurrentUICulture = CultureInfo.CurrentCulture;
      Thread.CurrentThread.CurrentCulture = CultureInfo.CurrentCulture;
    }

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
