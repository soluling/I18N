using System.Windows;

namespace Pages
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

    private void Button_Click(object sender, RoutedEventArgs e)
    {
      frame.Content = new Page1();
      //frame.Content = new Sub.Page1();
      //frame.Content = new Page2();
    }
  }
}
