using System.Windows;

namespace SinglePlural
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

    private string Process(int count)
    {
      return Soluling.MultiPattern.Format(Properties.Resources.MessagePlural, count, count);
    }

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      Soluling.Language.Id = Properties.Resources.Language;

      label0.Content = Process(0);
      label1.Content = Process(1);
      label2.Content = Process(2);
      label3.Content = Process(3);
      label4.Content = Process(4);
      label5.Content = Process(5);
      label11.Content = Process(11);
      label21.Content = Process(21);
      label101.Content = Process(101);
      label111.Content = Process(111);
    }
  }
}
