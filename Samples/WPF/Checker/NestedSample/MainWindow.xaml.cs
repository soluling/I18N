using System.Windows;

namespace NestedSample
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

    private void Window_ContentRendered(object sender, System.EventArgs e)
    {
      NewTool.WPF.WpfChecker.CheckRoot(this);
    }
  }
}
