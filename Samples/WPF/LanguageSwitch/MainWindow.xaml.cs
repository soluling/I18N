using System;
using System.Windows;
using Soluling;
using Soluling.WPF;

namespace LanguageSwitch
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

    public LanguageName DisplaLanugage
    {
      get { return (LanguageName)languageComboBox.SelectedIndex; }
    }

    // This procedure initializes the properties that are set on run time
    private void UpdateItems()
    {
      label2.Content = Properties.Resources.String1;
    }

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      // Set the properties for first time
      UpdateItems();
    }

    private void languageButton_Click(object sender, RoutedEventArgs e)
    {
      // Show a language select dialog and turn on the selected language
      if (SelectLanguage.Select(DisplaLanugage))
      {
        // Language has been changed.
        // Properties that were set on run time must be reset.
        UpdateItems();

        // Store the language
        Properties.Settings.Default.Language = Soluling.Language.Culture.Name;
        Properties.Settings.Default.Save();
      }
    }
  }
}
