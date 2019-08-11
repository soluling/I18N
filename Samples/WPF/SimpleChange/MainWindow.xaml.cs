using System.Windows;
using Soluling.WPF;

namespace SimpleChange
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

    private void Window_MouseDoubleClick(object sender, System.Windows.Input.MouseButtonEventArgs e)
    {
      SelectLanguage.Select();
    }
  }
}
