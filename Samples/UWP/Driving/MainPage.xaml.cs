using System;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Popups;
using Windows.ApplicationModel.Resources;

namespace Driving
{
  /// <summary>
  /// An empty page that can be used on its own or navigated to within a Frame.
  /// </summary>
  public sealed partial class MainPage : Page
  {
    private ResourceLoader rl = new ResourceLoader();

    public MainPage()
    {
      InitializeComponent();
    }

    private double Distance
    {
      get
      {
        try
        {
          return Convert.ToDouble(distanceTextBox.Text);
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
          return Convert.ToDouble(speedTextBox.Text);
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
    }

    private void textBox_TextChanged(object sender, TextChangedEventArgs e)
    {
      UpdateItems();
    }

    private void calculateButton_Click(object sender, RoutedEventArgs e)
    {
      var time = Distance/Speed;
      var hours = (int)time;
      var minutes = (int)Math.Round(60*(time - hours));

      resultLabel.Text = Soluling.MultiPattern.FormatMulti(rl.GetString("ResultPlural"), hours, minutes);
    }

    private async void aboutButton_Click(object sender, RoutedEventArgs e)
    {
      await new MessageDialog(rl.GetString("About")).ShowAsync();
    }

    private void exitButton_Click(object sender, RoutedEventArgs e)
    {
      App.Current.Exit();
    }
  }
}
