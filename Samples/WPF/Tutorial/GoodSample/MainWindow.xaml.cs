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
      // Use Plural.Format instead of String.Format whenever the placeholder contains a number
      return NewTool.MultiPattern.Format(Properties.Resources.SkisPlural, count);
    }

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      sampleLabel.Content = Properties.Resources.SampleText;
      dateLabel.Content = String.Format(Properties.Resources.TodayIs, DateTime.Now.ToShortDateString());
      
      skiLabel1.Content = GetSkiCount(1);
      skiLabel2.Content = GetSkiCount(2);
    }

    private void fileButton_Click(object sender, RoutedEventArgs e)
    {
      OpenFileDialog dialog = new OpenFileDialog();

      dialog.Title = Properties.Resources.OpenTitle;

      // This is safer than putting the whole filter into single resource string because only description texts are exposed to translator
      dialog.Filter = new NewTool.DialogFilter()
        .AddSupported(Properties.Resources.AllSupportedFiles)
        .Add(Properties.Resources.XmlFiles, "xml")
        .Add(Properties.Resources.IniFiles, "ini")
        .AddAll(Properties.Resources.AllFiles)
        .ToString();

      if (dialog.ShowDialog() == true)
        fileTextBox.Text = dialog.FileName;
    }
  }
}
