using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

public partial class Index : System.Web.UI.Page
{
  private double Distance
  {
    get
    {
      try
      {
        return System.Convert.ToDouble(distanceTextBox.Text);
      }
      catch
      {
        return -1;
      }
    }
  }

  private double Speed
  {
    get
    {
      try
      {
        return System.Convert.ToDouble(speedTextBox.Text);
      }
      catch
      {
        return -1;
      }
    }
  }
  
  protected void Page_Load(object sender, EventArgs e)
  {
  }

  protected void calculateButton_Click(object sender, EventArgs e)
  {
    if ((Distance >= 0) && (Speed > 0))
    {
      double time = Distance/Speed;
      uint hours = (uint)time;
      uint minutes = (uint)Math.Round(60 * (time - hours));

      resultLabel.Text = "Driving time is";  //loc
      hoursLabel.Text = String.Format("{0} hours", hours);  //loc: This is a message pattern and {0} must exists in the localized string
      minutesLabel.Text = String.Format("{0} minutes", minutes);  //loc: This is a message pattern and {0} must exists in the localized string
    }
    else
    {
      resultLabel.Text = "You must enter a valid distance and a speed!";  //loc
    }
  }
}