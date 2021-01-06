using System.Windows;

namespace LanguageSwitch
{
  /// <summary>
  /// Interaction logic for App.xaml
  /// </summary>
  public partial class App : Application
  {
    public App()
    {
      Soluling.Language.SetInitial(LanguageSwitch.Properties.Settings.Default.Language);
    }
  }
}
