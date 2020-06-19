using System;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Input;
// Add these
using Windows.ApplicationModel.Resources;
using Windows.Globalization.DateTimeFormatting;
using Windows.System.UserProfile;

namespace Tutorial
{
  /// <summary>
  /// An empty page that can be used on its own or navigated to within a Frame.
  /// </summary>
  public sealed partial class MainPage : Page
  {
    // This provides string resource file for the application
    private ResourceLoader rl = new ResourceLoader();

    public MainPage()
    {
      InitializeComponent();
    }

    private void Page_Loaded(object sender, RoutedEventArgs e)
    {
      var name = "John";
      var today = DateTime.Today;

      // Replace hard coded strings with resource strings
      text2.Text = rl.GetString("Sample");

      // Use String.Format instead of ".." + value in order to make it possible to localize dynamic messages correctly
      text3.Text = String.Format(rl.GetString("Name"), name);

      // Use DateTimeFormatter to show date as string
      var formatter = new DateTimeFormatter("shortdate", GlobalizationPreferences.Languages);
      text4.Text = String.Format(rl.GetString("Today"), formatter.Format(today));

      // Initializes the plural engine to use the current language
      Soluling.Language.Id = rl.GetString("Language");
      SetOranges(2);
    }

    private void Page_PointerPressed(object sender, PointerRoutedEventArgs e)
    {
      SetOranges(1);
    }

    private void SetOranges(int count)
    {
      // Use Plural.Format instead of String.Format when you want to have grammatically correct sentences
      text5.Text = Soluling.MultiPattern.Format(rl.GetString("OrangesPlural"), count);
    }
  }
}
