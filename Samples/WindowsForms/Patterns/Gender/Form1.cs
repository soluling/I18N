using System;
using System.Windows.Forms;
using System.Drawing;
using Soluling;
using Soluling.Forms;

namespace SimpleGender
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Process(Gender gender, string name, Label messageLabel, Label infoLabel)
    {
      // Update message label
      messageLabel.Text = MultiPattern.Format(Properties.Resources.OtherMessageGender, gender, name);

      // Update info label
      var actualGender = MultiPattern.GetGender(Properties.Resources.OtherMessageGender, gender);

      if (actualGender != gender)
        infoLabel.Font = new Font(infoLabel.Font, FontStyle.Bold);
      else
        infoLabel.Font = new Font(infoLabel.Font, FontStyle.Regular);

      infoLabel.Text = PatternNames.GetGenderName(actualGender);
    }

    private void UpdateItems()
    {
      Process(Gender.Male, "John", maleLabel, maleInfo);
      Process(Gender.Female, "Jill", femaleLabel, femaleInfo);
      Process(Gender.Neutral, "Amazon", neutralLabel, neutralInfo);
    }

    private void Form1_Load(object sender, EventArgs e)
    {
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
