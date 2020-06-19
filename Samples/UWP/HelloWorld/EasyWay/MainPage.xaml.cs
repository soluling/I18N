using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.ApplicationModel.Resources;

namespace HelloWorld
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

    private void Page_Loaded_1(object sender, RoutedEventArgs e)
    {
      var rl = new ResourceLoader();
      text2.Text = rl.GetString("HowAreYou");
    }
  }
}
