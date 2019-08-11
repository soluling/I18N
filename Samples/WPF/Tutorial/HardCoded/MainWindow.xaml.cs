using System;
using System.Windows;
using System.Windows.Controls;

namespace HardCoded
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
      label1.Content = "This is a sample";
    }
  }
}
