using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Drawing;
using NewTool;

namespace Driving
{
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
      NewTool.Plural.SetLanguage(Driving.Resources.Language);
    }

    protected void calculateButton_Click(object sender, EventArgs e)
    {
      if ((Distance >= 0) && (Speed > 0))
      {
        double time = Distance / Speed;
        uint hours = (uint)time;
        uint minutes = (uint)Math.Round(60 * (time - hours));

        resultLabel.Text = MultiPattern.FormatMulti(Driving.Resources.ResultPlural, hours, minutes);
      }
      else
      {
        resultLabel.Text = Driving.Resources.YouMustEnterValue;
      }
    }
  }
}