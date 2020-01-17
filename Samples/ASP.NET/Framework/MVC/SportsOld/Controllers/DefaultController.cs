using System.Web.Mvc;
using Sports.Models;

namespace Sports.Controllers
{
  public class DefaultController : Controller
  {
    public ActionResult Index()
    {
      var model = new Sport
      {
        Name = "Ice Hockey",
        FieldPlayers = 5,
        Goalie = true,
        Origin = "Canada"
      };

      return View(model);
    }
  }
}