using System.Windows;

namespace Test
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

    private void button1_Click(object sender, RoutedEventArgs e)
    {
      new Window1().Show();
    }

    private void button2_Click(object sender, RoutedEventArgs e)
    {
      new Window2().Show();
    }

    private void button3_Click(object sender, RoutedEventArgs e)
    {
      new Window3().Show();
    }
  }
}
