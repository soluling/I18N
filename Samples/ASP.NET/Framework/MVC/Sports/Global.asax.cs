using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using System.Web.Routing;
using System.Data.Entity;

namespace Sports
{
  public class MvcApplication : System.Web.HttpApplication
  {
    protected void Application_Start()
    {
      Database.SetInitializer<Sports.Models.SportContext>(null);
      AreaRegistration.RegisterAllAreas();
      RouteConfig.RegisterRoutes(RouteTable.Routes);
    }
  }
}
