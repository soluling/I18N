using System;
using System.Windows;
using NewTool.WPF;

namespace TabSample
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
      WpfChecker.CheckRoot(this);
    }
  }
}
