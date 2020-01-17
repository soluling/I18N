using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Localization;
using Microsoft.Extensions.Logging;
using Soluling.Sport;

namespace RazorSport.Pages
{
  public class EditModel : SportModel
  {
    private readonly ILogger<EditModel> _logger;
    private readonly IStringLocalizer _localizer;

    public EditModel(SportService sportService, ILogger<EditModel> logger, IStringLocalizer<EditModel> localizer) : base(sportService)
    {
      _logger = logger;
      _localizer = localizer;
    }

    public async Task<IActionResult> OnGet(int id)
    {
      Value = await _sportService.GetAsync(id, ActiveLanguage);
      return Page();
    }

    public IActionResult OnPost()
    {
      return Page();
    }

    public async Task<IActionResult> OnPostEdit()
    {
      if (!ModelState.IsValid)
        return Page();

      await _sportService.EditAsync(Value);
      _logger.LogInformation($"Sport {Value.Id} edited.");

      Message = _localizer["Sport updated successfully"];

      return RedirectToPage("./Index");
    }
  }
}