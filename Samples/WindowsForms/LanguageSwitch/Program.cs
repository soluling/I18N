using System;
using System.Windows.Forms;
using NewTool;

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
      // Original language is English.
      // Default language is the current selection in Regional Settings sheet of Control panel.
      Resources.SetInitialLanguage("en");

      Application.EnableVisualStyles();
      Application.SetCompatibleTextRenderingDefault(false);
      Application.Run(new Form1());
    }
  }
}
