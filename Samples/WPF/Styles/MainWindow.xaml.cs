using System.Windows;

namespace Styles
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

    private void Button1_Click(object sender, RoutedEventArgs e)
    {
      new Window1().Show();
    }

    private void Button2_Click(object sender, RoutedEventArgs e)
    {
      new Window2().Show();
    }
  }
}
