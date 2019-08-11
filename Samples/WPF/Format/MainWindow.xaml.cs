using System;
using System.Windows;

namespace Format
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
      var name = "John";
      var name2 = "Jill";
      var name3 = "Bill";

      helloLabel.Content = String.Format(Properties.Resources.Hello, name);
      hello2Label.Content = String.Format(Properties.Resources.Hello2, name, name2);
      hello3Label.Content = String.Format(Properties.Resources.Hello3, name, name2, name3);

      var city = "Berlin";
      var city2 = "London";

      tripLabel.Content = String.Format(Properties.Resources.Trip, city);
      trip2Label.Content = String.Format(Properties.Resources.Trip2, city, city2);
    }
  }
}
