using System;
using System.Globalization;
using System.Threading;
using System.Windows.Forms;

namespace Core
{
  internal static class Program
  {
    /// <summary>
    ///  The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main()
    {
      Thread.CurrentThread.CurrentUICulture = CultureInfo.CurrentCulture;
      Thread.CurrentThread.CurrentCulture = CultureInfo.CurrentCulture;

      Application.SetHighDpiMode(HighDpiMode.SystemAware);
      Application.EnableVisualStyles();
      Application.SetCompatibleTextRenderingDefault(false);
      Application.Run(new Form1());
    }
  }
}
