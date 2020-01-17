using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Localization;
using Microsoft.Extensions.Logging;
using Soluling.Sport;

namespace RazorSport.Pages
{
  public class SetDefaultModel : SportModel
  {
    private readonly ILogger<SetDefaultModel> _logger;
    private readonly IStringLocalizer _localizer;

    public SetDefaultModel(SportService sportService, ILogger<SetDefaultModel> logger, IStringLocalizer<SetDefaultModel> localizer) : base(sportService)
    {
      _logger = logger;
      _localizer = localizer;
    }

    public IActionResult OnGet(int id)
    {
      return Page();
    }

    public async Task<IActionResult> OnPost()
    {
      await _sportService.SetDefaultAsync();
      _logger.LogInformation($"Default sports set.");
      Message = _localizer["Default sport successfully set!"];

      return RedirectToPage("./Index");
    }
  }
}