using System;
using System.Windows.Forms;

namespace Driving
{
  static class Program
  {
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main()
    {
      // Set the initial culture and UI culture to match the selected locale in Control Panel
      Soluling.Language.SetInitial();

      Application.EnableVisualStyles();
      Application.SetCompatibleTextRenderingDefault(false);
      Application.Run(new Form1());
    }
  }
}
