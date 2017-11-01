using System;
using System.Windows;

namespace CanvasSample
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

    private void Window_ContentRendered(object sender, EventArgs e)
    {
      NewTool.WPF.WpfChecker.CheckRoot(this);
      //NewTool.WPF.WpfChecker.CheckRoot(this, 3000);
    }
  }
}
