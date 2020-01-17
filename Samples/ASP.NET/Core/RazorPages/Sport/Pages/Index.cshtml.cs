using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Soluling.Sport;

namespace RazorSport.Pages
{
  public class IndexModel : SportModel
  {
    private readonly ILogger<IndexModel> _logger;

    [BindProperty]
    public List<Sport> Sports { get; set; }

    public IndexModel(SportService sportService, ILogger<IndexModel> logger): base(sportService)
    {
      _logger = logger;
    }

    public async Task OnGet()
    {
      Sports = (await _sportService.GetAllAsync(ActiveLanguage)).ToList();
    }
  }
}
