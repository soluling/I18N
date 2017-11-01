using System;
using System.Windows.Forms;
using NewTool.Forms;

namespace Select
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Process(string select, string name, Label label)
    {
      label.Text = NewTool.MultiPattern.Format(Properties.Resources.SportSelect, select, name);
    }

    // This procedure initializes the properties that are set on run time
    private void UpdateItems()
    {
      Process("soccer", "Pelé", soccerLabel);
      Process("hockey", "Gretzky", hockeyLabel);
      Process("basketball", "Jordan", basketballLabel);
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
