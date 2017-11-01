using System;
using System.Windows.Forms;

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
      NewTool.MultiPattern.SetLanguage(Properties.Resources.Language);
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

      hoursLabel.Text = NewTool.MultiPattern.Format(Properties.Resources.HoursPlural, hours, hours);
      minutesLabel.Text = NewTool.MultiPattern.Format(Properties.Resources.MinutesPlural, minutes, minutes);
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
