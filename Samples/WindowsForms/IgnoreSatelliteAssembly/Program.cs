using System;
using System.Windows.Forms;
using System.Threading;
using System.Globalization;

namespace IgnoreSatelliteAssembly
{
  static class Program
  {
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main()
    {
      // This disables satellite assembly files
      Thread.CurrentThread.CurrentUICulture = new CultureInfo("");

      Application.EnableVisualStyles();
      Application.SetCompatibleTextRenderingDefault(false);
      Application.Run(new Form1());
    }
  }
}
