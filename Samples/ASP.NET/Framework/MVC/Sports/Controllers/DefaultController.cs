using System.Web.Mvc;
using System.Linq;
using Sports.Models;

namespace Sports.Controllers
{
  public class DefaultController : Controller
  {
    public ActionResult Index(int id)
    {
      var sportContext = new SportContext();
      var model = sportContext.Sports.Single(sport => sport.Id == id);

      return View(model);
    }
  }
}