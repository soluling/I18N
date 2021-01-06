using System;
using System.Windows.Forms;

namespace LanguageSwitch
{
  static class Program
  {
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main()
    {
      // Set the original and default languages.
      // Default language is the current selection in Regional Settings sheet of Control panel.
      Soluling.Language.SetInitial(Properties.Settings.Default.Language);

      Application.EnableVisualStyles();
      Application.SetCompatibleTextRenderingDefault(false);
      Application.Run(new Form1());
    }
  }
}
