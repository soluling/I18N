using System.Windows;
using System.Windows.Media.Imaging;

namespace Image
{
  /// <summary>
  /// Interaction logic for MainWindow.xaml
  /// </summary>
  public partial class MainWindow : Window
  {
    public MainWindow()
    {
      InitializeComponent();
    }

    private BitmapImage GetImage(string name)
    {
      // name image was added as Embedded Resource. XAML cannot directly access it (need to be added as Resource).
      // This is why we need to programatically load the image.
      var image = new BitmapImage();
      image.BeginInit();
      image.StreamSource = Soluling.Language.GetEmbeddedResource(name);
      image.EndInit();

      return image;
    }

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      embeddedResourceImage.Source = GetImage("add.png");

      // Note! "Sub/add.png" file becomes "Sub.add.png" resource when compiled
      embeddedResourceImageSub.Source = GetImage("Sub.add.png");
    }
  }
}
