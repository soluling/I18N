using System.Globalization;

namespace Simple2
{
  public partial class App : Application
  {
    public App()
    {
      Thread.CurrentThread.CurrentUICulture = CultureInfo.CurrentCulture;
      Thread.CurrentThread.CurrentCulture = CultureInfo.CurrentCulture;

      InitializeComponent();

      MainPage = new AppShell();
    }
  }
}