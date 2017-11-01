using System.Windows;

namespace SystemString
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

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      // Use application resource
      label1.Content = (string)Application.Current.FindResource("one");

      // Use local resource
      label3.Content = (string)FindResource("three");
    }
  }
}
