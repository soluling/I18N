using System.Globalization;
using System.Threading;
using System.Web.Mvc;
using System.Web.Routing;

namespace LocalizedViews
{
    public class MvcApplication : System.Web.HttpApplication
    {
        protected void Application_Start()
        {
            AreaRegistration.RegisterAllAreas();
            RouteConfig.RegisterRoutes(RouteTable.Routes);

            ViewEngines.Engines.Clear();
            ViewEngines.Engines.Add(new NewTool.MVC.LocalizedRazorViewEngine());

            //Thread.CurrentThread.CurrentCulture = CultureInfo.CurrentCulture;
            //Thread.CurrentThread.CurrentUICulture = CultureInfo.CurrentCulture;
        }

        protected void Application_BeginRequest()
        {
        }
  }
}
