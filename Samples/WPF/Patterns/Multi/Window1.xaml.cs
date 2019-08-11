using System.Windows;

namespace MultiPlural
{
  /// <summary>
  /// Interaction logic for Window1.xaml
  /// </summary>
  public partial class Window1 : Window
  {
    public Window1()
    {
      InitializeComponent();
    }

    public bool UseZero { get { return useZeroCheck.IsChecked.Value; } }

    private string Process(int completed, int total)
    {
      if (UseZero)
        return Soluling.MultiPattern.FormatMulti(Properties.Resources.ZeroMessagePlural, completed, total);
      else
        return Soluling.MultiPattern.FormatMulti(Properties.Resources.MessagePlural, completed, total);
    }

    private void UpdateValues()
    {
      label0_0.Content = Process(0, 0);
      label0_1.Content = Process(0, 1);
      label1_1.Content = Process(1, 1);
      label0_2.Content = Process(0, 2);
      label1_2.Content = Process(1, 2);
      label2_2.Content = Process(2, 2);
      label1_3.Content = Process(1, 3);
      label2_3.Content = Process(2, 3);
      label1_10.Content = Process(1, 10);
      label5_10.Content = Process(5, 10);
    }

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      Soluling.Language.Id = Properties.Resources.Language;
      UpdateValues();
    }

    private void useZeroCheck_Checked(object sender, RoutedEventArgs e)
    {
      UpdateValues();
    }
  }
}
