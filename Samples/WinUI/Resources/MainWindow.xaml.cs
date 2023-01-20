using System.IO;
using Microsoft.UI.Xaml;
using Microsoft.UI.Xaml.Controls;
using Microsoft.Windows.ApplicationModel.Resources;

namespace Resources
{
  /// <summary>
  /// An empty window that can be used on its own or navigated to within a Frame.
  /// </summary>
  public sealed partial class MainWindow : Window
  {
    ResourceManager resManager;
    ResourceContext context;
    ResourceLoader rl;

    public MainWindow()
    {
      this.InitializeComponent();

      resManager = new ResourceManager("resources.pri");
      context = resManager.CreateResourceContext();
      rl = new ResourceLoader();

      textBlock2.Text = rl.GetString("String1");

      ReadTextFile(textBlock3, "Simple.xml");
      ReadTextFile(textBlock4, "Simple.txt");
      ReadTextFile(textBlock5, "Plain.txt");
    }

    private void ReadTextFile(TextBlock textBlock, string name)
    {
      var resource = resManager.MainResourceMap.GetValue("Files/" + name, context);
      var path = resource.ValueAsString;
      textBlock.Text = File.ReadAllText(path);
    }

    private void button1_Click(object sender, RoutedEventArgs e)
    {
      var resource = resManager.MainResourceMap.GetValue("Files/sound.wav", context);
      var path = resource.ValueAsString;

      var player = new System.Media.SoundPlayer(path);
      player.Play();
    }
  }
}
