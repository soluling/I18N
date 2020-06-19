// This sample shows how to create grammatically correct dynamic messages
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.ApplicationModel.Resources;

namespace Plural
{
  /// <summary>
  /// An empty page that can be used on its own or navigated to within a Frame.
  /// </summary>
  public sealed partial class MainPage : Page
  {
    private ResourceLoader rl = new ResourceLoader();

    public MainPage()
    {
      InitializeComponent();
    }

    private string Process(int count)
    {
      // This is similar to String.Format(rl.GetString("MessagePlural"), count)
      return Soluling.MultiPattern.Format(rl.GetString("MessagePlural"), count);
    }

    private void Page_Loaded(object sender, RoutedEventArgs e)
    {
      // Set the language of plural engine
      Soluling.Language.Id = rl.GetString("Language");

      // Set the file texts on various file amount values
      textBlock0.Text = Process(0);
      textBlock1.Text = Process(1);
      textBlock2.Text = Process(2);
      textBlock3.Text = Process(3);
      textBlock4.Text = Process(4);
      textBlock5.Text = Process(5);
      textBlock11.Text = Process(11);
      textBlock21.Text = Process(21);
      textBlock101.Text = Process(101);
      textBlock111.Text = Process(111);
    }
  }
}
