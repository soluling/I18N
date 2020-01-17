using System.Collections.Generic;
using System.Globalization;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Localization;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Localization;

namespace Minimal
{
  public class Startup
  {
    public void ConfigureServices(IServiceCollection services)
    {
      // 1) Specify .resx directory. Add Resources directory and add Startup.*.resx files.
      services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });
    }

    // 2) Add IStringLocalizer<Startup> parameter so it can be injected. 
    public void Configure(IApplicationBuilder app, IStringLocalizer<Startup> localizer, IHostingEnvironment env, ILoggerFactory loggerFactory)
    {
      if (env.IsDevelopment())
        app.UseDeveloperExceptionPage();

      // 3) Add supported languages: Finnish and German
      var supportedCultures = new List<CultureInfo>
      {
        new CultureInfo("fi"),
        new CultureInfo("de")
      };

      var options = new RequestLocalizationOptions
      {
        DefaultRequestCulture = new RequestCulture("en"),
        SupportedCultures = supportedCultures,
        SupportedUICultures = supportedCultures
      };

      app.UseRequestLocalization(options);

      app.Run(async (context) =>
      {
        // 4) Use the localizer. Add "Hello World!" to Startup.resx and localize Startup.resx to Finnish (Startup.fi.resx) and German (Startup.de.resx)
        await context.Response.WriteAsync(localizer["Hello World!"]);
      });
    }
  }
}
