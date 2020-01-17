using System.Web.Mvc;
using Original.Models;

namespace Original.Controllers
{
  public class DefaultController : Controller
  {
    [HttpGet]
    public ActionResult Index()
    {
      return View();
    }

    [HttpPost]
    public ActionResult Index(DrivingModel model)
    {
      return View(model);
    }
  }
}