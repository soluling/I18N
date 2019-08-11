using System;
using System.Windows.Forms;
using Soluling;

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
      // Set the language of the application. The plural engine must know it.
      //Language.Id = Properties.Resources.Language;
      UpdateItems();
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
