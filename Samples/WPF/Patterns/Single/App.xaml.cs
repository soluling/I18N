using System.Windows;

namespace SinglePlural
{
  /// <summary>
  /// Interaction logic for App.xaml
  /// </summary>
  public partial class App : Application
  {
    public App()
    {
      Soluling.Language.SetInitial();
    }
  }
}
