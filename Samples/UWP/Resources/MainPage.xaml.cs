using System;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.ApplicationModel.Resources;
using Windows.Storage;
using Windows.Storage.Streams;

// The Blank Page item template is documented at http://go.microsoft.com/fwlink/?LinkId=234238

namespace Resources
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

    private async void ReadTextFile(TextBlock textBlock, string name)
    {
      IBuffer buffer = await PathIO.ReadBufferAsync("ms-appx:///" + name);

      using (DataReader dataReader = DataReader.FromBuffer(buffer))
        textBlock.Text = dataReader.ReadString(buffer.Length);
    }

    private void Page_Loaded(object sender, RoutedEventArgs e)
    {
      var rl = new ResourceLoader();

      textBlock2.Text = rl.GetString("String1");

      ReadTextFile(textBlock3, "Simple.xml");
      ReadTextFile(textBlock4, "Simple.txt");
      ReadTextFile(textBlock5, "Plain.txt");
    }

    private async void Button_Click(object sender, RoutedEventArgs e)
    {
      var uri = new System.Uri("ms-appx:///sound.wav");
      var file = await Windows.Storage.StorageFile.GetFileFromApplicationUriAsync(uri);
      var stream = await file.OpenReadAsync();
      var mediaElement = new MediaElement();

      mediaElement.SetSource(stream, file.ContentType);
      mediaElement.Play();
    }
  }
}
