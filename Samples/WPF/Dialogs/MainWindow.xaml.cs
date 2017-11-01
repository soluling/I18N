using System.Windows;
using NewTool;

namespace Dialogs
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

    private void openDialog_Click(object sender, RoutedEventArgs e)
    {
      var dialog = new Microsoft.Win32.OpenFileDialog();

      dialog.Filter = new DialogFilter().
        AddSupported(Properties.Resources.AllSupportedFiles). 
        Add(Properties.Resources.XmlFiles, "xml").
        Add(Properties.Resources.TextFiles, "txt").
        AddAll(Properties.Resources.AllFiles).
        ToString();

      dialog.DefaultExt = ".xml";

      if (dialog.ShowDialog() == true)
        MessageBox.Show(dialog.FileName);
    }

    private void openDialogWinForms_Click(object sender, RoutedEventArgs e)
    {
      // Same as above but using Windows Forms version of open dialog
      var dialog = new System.Windows.Forms.OpenFileDialog();

      dialog.Filter = new DialogFilter().
        AddSupported(Properties.Resources.AllSupportedFiles). 
        Add(Properties.Resources.XmlFiles, "xml").
        Add(Properties.Resources.TextFiles, "txt").
        AddAll(Properties.Resources.AllFiles).
        ToString();

      dialog.DefaultExt = ".xml";

      if (dialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
        System.Windows.Forms.MessageBox.Show(dialog.FileName);
    }

    private void saveDialog_Click(object sender, RoutedEventArgs e)
    {
      var dialog = new Microsoft.Win32.SaveFileDialog();
      
      dialog.Filter = new DialogFilter().
        Add(Properties.Resources.XmlFiles, "xml"). 
        Add(Properties.Resources.TextFiles, "txt").
        ToString();

      dialog.FileName = Properties.Resources.SaveFileName;
      dialog.DefaultExt = ".xml";

      if (dialog.ShowDialog() == true)
        MessageBox.Show(dialog.FileName);
    }

    private void saveDialogWinForms_Click(object sender, RoutedEventArgs e)
    {
      var dialog = new System.Windows.Forms.SaveFileDialog();
      
      dialog.Filter = new DialogFilter().
        Add(Properties.Resources.XmlFiles, "xml").
        Add(Properties.Resources.TextFiles, "txt").
        ToString();

      dialog.FileName = Properties.Resources.SaveFileName;
      dialog.DefaultExt = ".xml";

      if (dialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
        System.Windows.Forms.MessageBox.Show(dialog.FileName);
    }

    private void folderDialog_Click(object sender, RoutedEventArgs e)
    {
      var dialog = new System.Windows.Forms.FolderBrowserDialog();
      dialog.Description = Properties.Resources.FolderDescription;
      dialog.ShowNewFolderButton = true;

      if ((dialog.ShowDialog() == System.Windows.Forms.DialogResult.OK) && !string.IsNullOrEmpty(dialog.SelectedPath))
        MessageBox.Show(dialog.SelectedPath);
    }
  }
}
