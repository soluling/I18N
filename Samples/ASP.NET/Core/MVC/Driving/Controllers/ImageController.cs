using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc;
using Soluling.AspNet;

namespace RazorDriving.Controllers
{
  [Route("localizedimages")]
  [ApiController]
  public class ImageController : ControllerBase
  {
    private readonly IHostingEnvironment hostingEnvironment;

    public ImageController(IHostingEnvironment hostingEnvironment)
    {
      this.hostingEnvironment = hostingEnvironment;
    }

    [HttpGet]
    [Route("{name}")]
    public IActionResult GetFlag(string name)
    {
      return this.GetImage(hostingEnvironment.WebRootPath, name);
    }
  }
}