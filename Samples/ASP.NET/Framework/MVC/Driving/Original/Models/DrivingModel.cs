using System;
using System.ComponentModel.DataAnnotations;

namespace Original.Models
{
  public class DrivingModel
  {
    [Required(ErrorMessage = "Required")]
    [Display(Name = "Distance:")]
    public string Distance { get; set; }

    [Required(ErrorMessage = "Required")]
    [Display(Name = "Speed:")]
    public string Speed { get; set; }

    public bool HasValues
    {
      get { return (Distance != null) && (Speed != null); }
    }

    private double GetTime()
    {
      double distance = Convert.ToDouble(Distance);
      double speed = Convert.ToDouble(Speed);

      return distance/speed;
    }

    public string Message
    {
      get
      {
        if (HasValues)
          return "Driving Time is";
        else
          return "";
      }
    }

    public string HoursMessage
    {
      get
      {
        if (HasValues)
        {
          uint hours = (uint)GetTime();

          return String.Format("{0} hours", hours);
        }
        else
          return "";
      }
    }

    public string MinutesMessage
    {
      get
      {
        if (HasValues)
        {
          double time = GetTime();
          uint hours = (uint)time;
          uint minutes = (uint)Math.Round(60 * (time - hours));

          return String.Format("{0} minutes", minutes);
        }
        else
          return "";
      }
    }
  }
}
