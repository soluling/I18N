using Microsoft.UI;
using Microsoft.UI.Windowing;
using Microsoft.UI.Xaml;
using System;

namespace Tutorial
{
  /// <summary>
  /// An empty window that can be used on its own or navigated to within a Frame.
  /// </summary>
  public sealed partial class MainWindow : Window
  {
    int count = 0;

    public MainWindow()
    {
      this.InitializeComponent();

      var name = "John";
      var today = DateTime.Today;

      IntPtr hWnd = WinRT.Interop.WindowNative.GetWindowHandle(this);
      WindowId windowId = Win32Interop.GetWindowIdFromWindow(hWnd);
      AppWindow appWindow = AppWindow.GetFromWindowId(windowId);

      var size = new Windows.Graphics.SizeInt32();
      size.Width = 300;
      size.Height = 200;

      appWindow.Resize(size);
      appWindow.Title = "Click Sample";

      text2.Text = "This is a sample";
      text3.Text = "My name is " + name;
      text4.Text = "Today is " + today.Month + "/" + today.Day + "/" + today.Year % 100;
    }

    private void myButton_Click(object sender, RoutedEventArgs e)
    {
      count++;

      if (count == 1)
        myButton.Content = $"Clicked {count} time";
      else
        myButton.Content = $"Clicked {count} times";
    }
  }
}
