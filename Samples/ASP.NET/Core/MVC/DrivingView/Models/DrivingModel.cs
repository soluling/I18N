using Microsoft.AspNetCore.Mvc;

namespace DrivingView.Models
{
  public class DrivingModel
  {
    [BindProperty]
    public double Distance { get; set; } = 100;

    [BindProperty]
    public double Speed { get; set; } = 55;

    public string Message { get; set; }
  }
}
