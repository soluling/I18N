using System.Windows;
using System.Windows.Markup;
using System.Threading;
using System.Globalization;

namespace Binding
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

      // The formatting in the data binding (e.g. StringFormat) does not use the CurrentCulture.
      // We have two choises. First is to let Soluling add Language property to each localized XAML file.
      // This is the method we currently use. However we can also do that in code.
      // The following code sets the Language property of all WPF elements.
      /*
      FrameworkElement.LanguageProperty.OverrideMetadata(
        typeof(FrameworkElement), 
        new FrameworkPropertyMetadata(XmlLanguage.GetLanguage(CultureInfo.CurrentCulture.Name)));
      */
    }
  }
}
