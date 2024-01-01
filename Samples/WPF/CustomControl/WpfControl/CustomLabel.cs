using System.Windows;
using System.Windows.Controls;

namespace WpfControl
{
  public class CustomLabel : Label
  {
    public static DependencyProperty TextValueProperty;

    static CustomLabel()
    {
      TextValueProperty = DependencyProperty.Register("TextValue", typeof(string), typeof(CustomLabel));
    }

    public string TextValue
    {
      get { return (string)GetValue(TextValueProperty); }
      set { SetValue(TextValueProperty, value); }
    }
  }
}
