// http://blog.greatrexpectations.com/2012/11/20/localising-mvc-views-using-display-modes/

using System.Globalization;
using System.IO;
using System.Web.Mvc;
using System.Web;

namespace NewTool.MVC
{
  public class Utilities
  {
    static public string GetBrowserLanguage(HttpRequestBase request)
    {
      var userLanguages = request.UserLanguages;

      CultureInfo ci;

      if (userLanguages.Length > 0)
      {
        try
        {
          ci = new CultureInfo(userLanguages[0]);
        }
        catch (CultureNotFoundException)
        {
          ci = CultureInfo.InvariantCulture;
        }
      }
      else
        ci = CultureInfo.InvariantCulture;

      return ci.Name;
    }

    public static string GetImageFile(HttpServerUtilityBase server, string fileName, string culture = "")
    {
      string localizedImage = "\\" + Path.Combine(Path.Combine(Path.GetDirectoryName(fileName), culture), Path.GetFileName(fileName));

      if (File.Exists(server.MapPath(localizedImage)))
        return localizedImage;
      else
      {
        int index = culture.IndexOf('-');

        if (index > 0)
        {
          string language = culture.Substring(0, index);

          localizedImage = "\\" + Path.Combine(Path.Combine(Path.GetDirectoryName(fileName), language), Path.GetFileName(fileName));

          if (File.Exists(server.MapPath(localizedImage)))
            return localizedImage;
          else
            return fileName;
        }
        else
          return fileName;
      }
    }
  }

  public class LocalizedRazorViewEngine : RazorViewEngine
  {
    public override ViewEngineResult FindPartialView(ControllerContext controllerContext, string partialViewName, bool useCache)
    {
      string localizedPartialViewName = partialViewName;

      if (!string.IsNullOrEmpty(partialViewName))
        localizedPartialViewName += "." + CultureInfo.CurrentCulture.Name;

      var result = base.FindPartialView(controllerContext, localizedPartialViewName, useCache);

      if (result.View == null)
        result = base.FindPartialView(controllerContext, partialViewName, useCache);

      return result;
    }

    public override ViewEngineResult FindView(ControllerContext controllerContext, string viewName, string masterName, bool useCache)
    {
      string localizedViewName = viewName;

      if (!string.IsNullOrEmpty(viewName))
        localizedViewName += "." + CultureInfo.CurrentCulture.Name;

      string localizedMasterName = masterName;

      if (!string.IsNullOrEmpty(masterName))
        localizedMasterName += "." + CultureInfo.CurrentCulture.Name;

      var result = base.FindView(controllerContext, localizedViewName, localizedMasterName, useCache);

      if (result.View == null)
        result = base.FindView(controllerContext, viewName, masterName, useCache);

      return result;
    }
  }
}
