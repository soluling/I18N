#define ZERO

using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

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

    private string Process(int completed, int total)
    {
#if ZERO 
      return NewTool.MultiPattern.FormatMulti(Properties.Resources.ZeroMessagePlural, completed, total);
#else
      return NewTool.MultiPattern.FormatMulti(Properties.Resources.MessagePlural, completed, total);
#endif
    }

    private void Window_Loaded(object sender, RoutedEventArgs e)
    {
      NewTool.MultiPattern.SetLanguage(Properties.Resources.Language);

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
  }
}
