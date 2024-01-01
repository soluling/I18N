using System.Windows;
using System.Windows.Controls;
using Soluling;

namespace GenderApp
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

    private void Process(Gender gender, string name, Label messageLabel, Label infoLabel)
    {
      // Update message label
      messageLabel.Content = MultiPattern.Format(Properties.Resources.OtherMessageGender, gender, name);

      // Update info label
      var actualGender = MultiPattern.GetGender(Properties.Resources.OtherMessageGender, gender);

      if (actualGender != gender)
        infoLabel.FontWeight = FontWeights.Bold;
      else
        infoLabel.FontWeight = FontWeights.Normal;

      infoLabel.Content = PatternNames.GetGenderName(actualGender);
    }

    private void UpdateItems()
    {
      Process(Gender.Male, "John", maleLabel, maleInfo);
      Process(Gender.Female, "Jill", femaleLabel, femaleInfo);
      Process(Gender.Neutral, "Amazon", neutralLabel, neutralInfo);
    }

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      UpdateItems();
    }
  }
}
