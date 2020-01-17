using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Localization;
using Microsoft.Extensions.Logging;
using Soluling.Sport;

namespace RazorSport.Pages
{
  public class DeleteModel : SportModel
  {
    private readonly ILogger<DeleteModel> _logger;
    private readonly IStringLocalizer _localizer;

    public DeleteModel(SportService sportService, ILogger<DeleteModel> logger, IStringLocalizer<DeleteModel> localizer) : base(sportService)
    {
      _logger = logger;
      _localizer = localizer;
    }

    public async Task<IActionResult> OnGet(int id)
    {
      Value = await _sportService.GetAsync(id, ActiveLanguage);

      if (Value == null)
        return NotFound();

      return Page();
    }

    public async Task<IActionResult> OnPost(int? id)
    {
      if (id == null)
        return NotFound();

      Value = await _sportService.GetAsync(id.Value, ActiveLanguage);

      if (Value != null)
      {
        await _sportService.DeleteAsync(id.Value);
        _logger.LogInformation($"Sport {id.Value} deleted.");
        Message = _localizer["Sport Deleted Successfully!"];
      }

      return RedirectToPage("./Index");
    }
  }
}