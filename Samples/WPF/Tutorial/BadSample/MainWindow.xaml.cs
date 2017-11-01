using System;
using System.Windows;
using Microsoft.Win32;

namespace Tutorial
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

    private string GetSkiCount(int count)
    {
      // Hard code string!
      // Not grammatically correct when count is 1!
      // This works only if count in in the middle of the sentence!
      return "I have " + count + " skis.";
    }

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      // Hard coded string!
      // String is too long for control and some of the string will be cut!
      label2.Content = "This is a sample";

      DateTime date = DateTime.Now;

      // Hard coded string!
      // Incorrect date to string conversion!
      label3.Content = "Todays is " + date.Month + "/" + date.Day + "/" + date.Year%100;

      label4.Content = GetSkiCount(1);
      label5.Content = GetSkiCount(2);
    }

    private void Button_Click(object sender, RoutedEventArgs e)
    {
      OpenFileDialog dialog = new OpenFileDialog();

      // Hard coded string!
      dialog.Title = "Open File";

      // Hard coded string!
      // Too complex string to translate!
      dialog.Filter = "All supported files (*.xml;*.ini)|*.xml;*.ini|XML files (*.xml)|*.xml|Ini files (*.ini)|*.ini|All files (*.*)|*.*";

      if (dialog.ShowDialog() == true)
        textBox1.Text = dialog.FileName;
    }
  }
}
