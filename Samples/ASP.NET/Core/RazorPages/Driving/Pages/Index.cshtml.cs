using System;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using Microsoft.Extensions.Localization;
using Soluling;

namespace RazorDriving.Pages
{
  public class IndexModel : PageModel
  {
    [BindProperty]
    public double Distance { get; set;} = 100;

    [BindProperty]
    public double Speed { get; set;} = 55;

    public string Message { get; set;}

    private IStringLocalizer localizer;

    public IndexModel(IStringLocalizer<IndexModel> localizer)
    {
      this.localizer = localizer;
    }

    public void OnGet()
    {
      Message = localizer["Enter values and click Calculate."];  //loc This is a comment
    }

    public void OnPost()
    {
      Message = "";

      if (Speed > 0)
      {
        double time = Distance/Speed;
        int hours = (int)time;
        int minutes = (int)Math.Round(60*(time - hours));

        Message = MultiPattern.FormatMulti(localizer["Driving time is{plural, zero { } one { {0} hour } other { {0} hours }}{plural, one {{0} minute} other {{0} minutes}}"], hours, minutes);  //loc 0: Hours or minutes
      }
    }
  }
}
