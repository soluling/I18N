using System;
using System.Windows;
using System.Windows.Media.Imaging;
using NewTool.WPF;

namespace Driving
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

    private double Distance
    {
      get 
      { 
        try
        {
          return System.Convert.ToDouble(distanceTextBox.Text); 
        }
        catch
        {
          return 0;
        }
      }
    }

    private double Speed
    {
      get 
      { 
        try
        {
          return System.Convert.ToDouble(speedTextBox.Text); 
        }
        catch
        {
          return 0;
        }
      }
    }

    private void UpdateItems()
    {
      calculateButton.IsEnabled = (Distance > 0) && (Speed > 0);
      calculateMenu.IsEnabled = calculateButton.IsEnabled;
    }

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      NewTool.MultiPattern.SetLanguage(Properties.Resources.Language);
      UpdateItems();
    }

    private void textBox_TextChanged(object sender, System.Windows.Controls.TextChangedEventArgs e)
    {
      UpdateItems();
    }

    private void calculateButton_Click(object sender, RoutedEventArgs e)
    {
      double time = Distance/Speed;
      int hours = (int)time;
      int minutes = (int)Math.Round(60*(time - hours));

      hoursLabel.Content = NewTool.MultiPattern.Format(Properties.Resources.HoursPlural, hours, hours);
      minutesLabel.Content = NewTool.MultiPattern.Format(Properties.Resources.MinutesPlural, minutes, minutes);
    }

    private void languageMenu_Click(object sender, RoutedEventArgs e)
    {
      // Show a language select dialog and turn on the selected language
      if (SelectLanguage.Select())
      {
        // Language has been changed.
        // Properties that were set on run time must be reset.
        UpdateItems();

        // Translator does not translate images. Let's translate them.
        flagImage.Source = null;
        carImage.Source = null;

        UpdateLayout();
        GC.Collect();

        flagImage.Source = new BitmapImage(new Uri("flag_united_kingdom.png", UriKind.Relative));
        carImage.Source = new BitmapImage(new Uri("car_sedan_blue.png", UriKind.Relative));
      }
    }

    private void aboutMenu_Click(object sender, RoutedEventArgs e)
    {
      MessageBox.Show(Properties.Resources.About);
    }

    private void exitMenu_Click(object sender, RoutedEventArgs e)
    {
      Close();
    }
  }
}
