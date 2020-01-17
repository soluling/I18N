using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Localization;

namespace HelloWorld.Controllers
{
  public class DefaultController : Controller
  {
    private readonly IStringLocalizer localizer;

    // 4) Add contructer that gets injected localizer
    public DefaultController(IStringLocalizer<DefaultController> localizer)
    {
      this.localizer = localizer;
    }

    // GET: /<controller>/
    public string Index()
    {
      // 5) Use localizer
      return localizer["Hello World!"];
    }
  }
}
