using System;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Input;

namespace Tutorial
{
  /// <summary>
  /// An empty page that can be used on its own or navigated to within a Frame.
  /// </summary>
  public sealed partial class MainPage : Page
  {
    public MainPage()
    {
      InitializeComponent();
    }

    private void Page_Loaded(object sender, RoutedEventArgs e)
    {
      var name = "John";
      var today = DateTime.Today;

      text2.Text = "This is a sample";
      text3.Text = "My name is " + name;
      text4.Text = "Today is " + today.Month + "/" + today.Day + "/" + today.Year % 100;
      SetOranges(2);
    }

    private void Page_PointerPressed(object sender, PointerRoutedEventArgs e)
    {
      SetOranges(1);
    }

    private void SetOranges(uint count)
    {
      text5.Text = "I have " + count + " orenges";
    }
  }
}
