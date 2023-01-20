using Microsoft.UI;
using Microsoft.UI.Windowing;
using Microsoft.UI.Xaml;
using Microsoft.Windows.ApplicationModel.Resources;
using System;
using Windows.Globalization.DateTimeFormatting;
using Windows.System.UserProfile;

namespace Tutorial
{
  /// <summary>
  /// An empty window that can be used on its own or navigated to within a Frame.
  /// </summary>
  public sealed partial class MainWindow : Window
  {
    // This provides string resource file for the application
    private ResourceLoader rl = new ResourceLoader();

    int count = 0;

    public MainWindow()
    {
      this.InitializeComponent();

      var name = "John";
      var today = DateTime.Today;

      IntPtr hWnd = WinRT.Interop.WindowNative.GetWindowHandle(this);
      WindowId windowId = Win32Interop.GetWindowIdFromWindow(hWnd);
      AppWindow appWindow = AppWindow.GetFromWindowId(windowId);

      var size = new Windows.Graphics.SizeInt32();
      size.Width = 300;
      size.Height = 200;

      appWindow.Resize(size);

      // Replace hard coded strings with resource strings
      appWindow.Title = rl.GetString("Caption");
      text2.Text = rl.GetString("Sample");

      // Use String.Format instead of ".." + value in order to make it possible to localize dynamic messages correctly
      text3.Text = String.Format(rl.GetString("Name"), name);

      // Use DateTimeFormatter to show date as string
      var formatter = new DateTimeFormatter("shortdate", GlobalizationPreferences.Languages);
      text4.Text = string.Format(rl.GetString("Today"), formatter.Format(today));

      // Initializes the plural engine to use the current language
      Soluling.Language.Id = rl.GetString("Language");
    }

    private void myButton_Click(object sender, RoutedEventArgs e)
    {
      count++;
      // Use MultiPattern.Format instead of String.Format when you want to have grammatically correct sentences
      myButton.Content = Soluling.MultiPattern.Format(rl.GetString("ClickedTimesPlural"), count);
    }
  }
}
