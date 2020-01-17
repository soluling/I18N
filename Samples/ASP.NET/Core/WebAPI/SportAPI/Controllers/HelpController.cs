using System.Net;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Localization;

// This implements the help endpoint. It returns instructions about available endpoints. The content has been localized and the localized strings
// locate in Controllers.HelpController.*.resx files
namespace SportAPI.Controllers
{
  [Route("/")]
  public class HelpController : Controller
  {
    private readonly IStringLocalizer<HelpController> localizer;

    public HelpController(IStringLocalizer<HelpController> localizer)
    {
      // Save the injected localizer so methods can use it.
      this.localizer = localizer;
    }

    [HttpGet]
    public ContentResult Get()
    {
      var root = Startup.Configuration["Root"];

      if (!string.IsNullOrEmpty(root) && root[root.Length - 1] != '\\')
        root += "\\";

      // Format the HTML response using the localized strings.
      return new ContentResult 
      {
        ContentType = "text/html; charset=utf-8",
        StatusCode = (int)HttpStatusCode.OK,
        Content = 
          $"<h2>{localizer["Sport API"]}</h2>" +
          $"<p>{localizer["This API implementes following endpoints."]}</p>" +
          "<table border=\"1\">" +
          "<tr>" +
          $"<th>{localizer["URI path"]}</th>" +
          $"<th>{localizer["Description"]}</th>" +
          "</tr>" +
          "<tr>" +
          $"<td><a href=\"{root}sports\">sports</a></td>" +
          $"<td>{localizer["View and edit sports."]}</td>" +
          "</tr>" +
          "</table>"
      };
    }
  }
}