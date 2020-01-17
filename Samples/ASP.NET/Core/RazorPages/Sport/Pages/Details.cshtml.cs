using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Soluling.Sport;

namespace RazorSport.Pages
{
  public class DetailsModel : SportModel
  {
    private readonly ILogger<DetailsModel> _logger;

    public DetailsModel(SportService sportService, ILogger<DetailsModel> logger) : base(sportService)
    {
      _logger = logger;
    }

    public async Task<IActionResult> OnGet(int id)
    {
      Value = await _sportService.GetAsync(id, ActiveLanguage);

      if (Value == null)
        return NotFound();

      return Page();
    }
  }
}