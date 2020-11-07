using System.Globalization;
using System.Collections.Generic;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Localization;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;

namespace HelloWorld
{
  public class Startup
  {
    public void ConfigureServices(IServiceCollection services)
    {
      services.AddControllersWithViews();

      // 1) Specify .resx directory. Add Resources directory and add Startup.resx file.
      services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });
    }

    public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
    {
      if (env.IsDevelopment())
        app.UseDeveloperExceptionPage();

      // 2) Add supported languages: Finnish and German
      var supportedCultures = new List<CultureInfo>
      {
        new CultureInfo("en"),
        new CultureInfo("fi"),
        new CultureInfo("de")
      };

      // 3) Configure application to use the 
      var options = new RequestLocalizationOptions
      {
        DefaultRequestCulture = new RequestCulture("en"),
        SupportedCultures = supportedCultures,
        SupportedUICultures = supportedCultures
      };

      app.UseRequestLocalization(options);

      app.UseRouting();

      app.UseEndpoints(endpoints =>
      {
        endpoints.MapControllerRoute(
          name: "default",
          pattern: "{controller=Default}/{action=Index}/{id?}");
      });
    }
  }
}
