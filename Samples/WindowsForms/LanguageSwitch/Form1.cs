/*
 *  A sample that shows how to perform a runtime language switch.
 */
using System;
using System.Windows.Forms;
using Soluling.Forms;

namespace LanguageSwitch
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    // This procedure initializes the properties that are set on run time
    private void UpdateItems()
    {
      label2.Text = Properties.Resources.String1;
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      // Set the properties for first time
      UpdateItems();
    }

    private void languageButton_Click(object sender, EventArgs e)
    {
      // Show a language select dialog and turn on the selected language
      if (SelectLanguage.Select())
      {
        // Language has been changed.
        // Properties that were set on run time must be reset.
        UpdateItems();
      }
    }
  }
}