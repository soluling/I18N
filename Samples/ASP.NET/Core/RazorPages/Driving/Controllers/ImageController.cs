using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc;
using Soluling.AspNet;

namespace RazorDriving.Controllers
{
  [Route("localizedimages")]
  [ApiController]
  public class ImageController : ControllerBase
  {
    private readonly IWebHostEnvironment environment;

    public ImageController(IWebHostEnvironment environment)
    {
      this.environment = environment;
    }

    [HttpGet]
    [Route("{name}")]
    public IActionResult GetFlag(string name)
    {
      return this.GetImage(environment.WebRootPath, name);
    }
  }
}