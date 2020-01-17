using System;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Localization;
using Driving.Models;
using Soluling;

namespace Driving.Controllers
{
  public class DefaultController : Controller
  {
    private IStringLocalizer localizer;

    public DefaultController(IStringLocalizer<DefaultController> localizer)
    {
      this.localizer = localizer;
    }

    [HttpGet]
    public IActionResult Index()
    {
      return View();
    }

    [HttpPost]
    public IActionResult Index(DrivingModel driving)
    {
      if (driving.Speed > 0)
      {
        var time = driving.Distance / driving.Speed;
        var hours = (int)time;
        var minutes = (int)Math.Round(60 * (time - hours));

        driving.Message = MultiPattern.FormatMulti(localizer["Driving time is{plural, zero { } one { {0} hour } other { {0} hours }}{plural, one {{0} minute} other {{0} minutes}}"], hours, minutes);  //loc 0: Hours or minutes
      }
      else
      {
        driving.Message = "";
      }

      return View(driving);
    }
  }
}
