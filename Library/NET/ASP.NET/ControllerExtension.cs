using System.Collections.Generic;
using System.IO;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Localization;
using Microsoft.AspNetCore.Mvc;

namespace Soluling.AspNet
{
  /// <summary>
  /// 
  /// </summary>
  public static class ControllerExtension
  {
    /// <summary>
    /// 
    /// </summary>
    /// <param name="ext"></param>
    /// <returns></returns>
    public static string ImageExtToContentType(string ext)
    {
      if (ext == "jpg")
        return "image/jpeg";
      else
        return "image/" + ext;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="controller"></param>
    /// <param name="hostingEnvironment"></param>
    /// <param name="name"></param>
    /// <returns></returns>
    public static IActionResult GetImage(this ControllerBase controller, IHostingEnvironment hostingEnvironment, string name)
    {
      var ext = Path.GetExtension(name).Replace(".", "");

      if (ext == "")
      {
        ext = "png";
        name = name + ".png";
      }

      var stack = new Stack<string>();
      var path = hostingEnvironment.WebRootPath + "\\images";

      stack.Push(path + "\\" + name);

      var requestCulture = controller.Request.HttpContext.Features.Get<IRequestCultureFeature>();
      var ids = requestCulture.RequestCulture.UICulture.ToString().Split('-');

      foreach (var id in ids)
      {
        path = path + "\\" + id;
        stack.Push(path + "\\" + name);
      }

      while (stack.Count > 0)
      {
        var imageFile = stack.Pop();

        if (System.IO.File.Exists(imageFile))
          return controller.PhysicalFile(imageFile, ImageExtToContentType(ext));
      }

      return controller.NotFound();
    }
  }
}
