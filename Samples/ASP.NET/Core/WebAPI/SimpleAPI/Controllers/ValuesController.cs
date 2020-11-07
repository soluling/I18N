using System.Collections.Generic;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Localization;

namespace SimpleAPI.Controllers
{
  [Route("[controller]")]
  public class ValuesController : Controller
  {
    private readonly List<string> values = new List<string>();

    public ValuesController(IStringLocalizer<ValuesController> localizer)
    {
      values.Add(localizer["One"]);
      values.Add(localizer["Two"]);
    }

    // GET values
    [HttpGet]
    public IEnumerable<string> GetAll()
    {
      return values.ToArray();
    }

    // GET values/5
    [HttpGet("{id}")]
    public IActionResult Get(int id)
    {
      if (id < values.Count)
        return Ok(values[id]);
      else
        return NotFound();
    }
  }
}
