using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using Microsoft.Extensions.Localization;
using Microsoft.Extensions.Logging;
using Soluling.Sport;

namespace RazorSport.Pages
{
  public class AddModel : SportModel
  {
    private readonly ILogger<AddModel> _logger;
    private readonly IStringLocalizer _localizer;

    public List<SelectListItem> Olympics { get; set; }

    public AddModel(ILogger<AddModel> logger, SportService sportService, IStringLocalizer<AddModel> localizer) : base(sportService)
    {
      _logger = logger;
      _localizer = localizer;

      Olympics = GetOlympicEnumList(localizer);
    }

    public IActionResult OnGet()
    {
      Value = new Sport();
      Value.Languages.Add(new SportLanguage());

      return Page();
    }

    public async Task<IActionResult> OnPost()
    {
      if (!ModelState.IsValid)
        return Page();

      await _sportService.AddAsync(Value);
      _logger.LogInformation($"Sport {Value.Id} added.");

      Message = _localizer["New sport added successfully"];

      return RedirectToPage("./Index");
    }
  }
}