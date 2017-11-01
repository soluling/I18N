/*
 * This application shows how to implement multilingual application with runtime language switch and
 * with multilingual database. 
 * 
 * The application uses row-localized Access database that locates on
 * ..\..\..\..\Access\SportImage\Row\Sport.accdb
 * 
 * Application uses satellite assembly files. If the use selects a new language, a new satellite assembly
 * files is loaded and the translalator object translates the forms of the appliction. Application uses
 * NewTool's .NET classes: Resources.cs, Translator.cs and SelectLanguage.cs. All three files have been
 * added as links to the project.
 * 
 * Read all comments in this file and Form1.cs.
 */

using System;
using System.Collections.Generic;
using System.Windows.Forms;
using NewTool;

namespace Database
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
