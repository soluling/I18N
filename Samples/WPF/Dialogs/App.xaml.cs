using System.Windows;
using System.Threading;
using System.Globalization;

namespace Dialogs
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
  }
}
