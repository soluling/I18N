using System.Windows;

namespace WpfApp1
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

    private void localButton_Click(object sender, RoutedEventArgs e)
    {
      new LocalWindow().Show();
    }

    private void linkedButton_Click(object sender, RoutedEventArgs e)
    {
      new WpfApp2.SharedWindow().Show();
    }

    private void linkedButton2_Click(object sender, RoutedEventArgs e)
    {
      new WpfApp2.SharedSub.SharedSubWindow().Show();
    }
  }
}
