using System;
using System.Windows.Forms;
using Soluling;
using Soluling.Forms;

namespace Driving
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private double Distance
    {
      get 
      { 
        try
        {
          return Convert.ToDouble(distanceTextBox.Text); 
        }
        catch
        {
          return 0;
        }
      }
    }

    private double Speed
    {
      get 
      { 
        try
        {
          return Convert.ToDouble(speedTextBox.Text); 
        }
        catch
        {
          return 0;
        }
      }
    }

    private void UpdateItems()
    {
      calculateButton.Enabled = (Distance > 0) && (Speed > 0);
      calculateToolStripMenuItem.Enabled = calculateButton.Enabled;
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      SetPluralLanguage();
      UpdateItems();
    }

    private void SetPluralLanguage()
    {
      Soluling.Language.Id = Properties.Resources.Language;
    }

    private void textBox_TextChanged(object sender, EventArgs e)
    {
      UpdateItems();
    }

    private void calculateButton_Click(object sender, EventArgs e)
    {
      double time = Distance/Speed;
      int hours = (int)time;
      int minutes = (int)Math.Round(60*(time - hours));

      // Use plural enabled multi pattern Format instead of the plain string.Format
      resultLabel.Text = MultiPattern.FormatMulti(Properties.Resources.ResultPlural, hours, minutes);
    }

    private void languageToolStripMenuItem_Click(object sender, EventArgs e)
    {
      // Show a language select dialog and turn on the selected language
      if (SelectLanguage.Select())
      {
        // Language has been changed. Update the language of the plural engine.
        SetPluralLanguage();

        // Translator does not translate images. Let's translate them.
        carPictureBox.Image = Properties.Resources.car_sedan_blue;
        flagPictureBox.Image = Properties.Resources.flag_great_britain;
      }
    }

    private void aboutToolStripMenuItem_Click(object sender, EventArgs e)
    {
      MessageBox.Show(Properties.Resources.About);
    }

    private void exitToolStripMenuItem_Click(object sender, EventArgs e)
    {
      Close();
    }
  }
}
