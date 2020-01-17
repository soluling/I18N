using System.Web.Mvc;
using FullForm.Models;

namespace FullForm.Controllers
{
  public class ContactController : Controller
  {
    [HttpGet]
    public ActionResult ContactForm()
    {
      return View();
    }

    [HttpPost]
    public ActionResult ContactForm(ContactModel model)
    {
      if (model.HasValue && ModelState.IsValid)
      {
        InsertContact(model.Name, model.Email, model.Comments);
        TempData["notice"] = "You form was submitted";
        return RedirectToAction("Index", "Home");
      }
      else
        return View("ContactForm");
    }

    private void InsertContact(string name, string email, string comments)
    {
    }
  }
}