using System;
using System.Collections.Generic;
using System.Globalization;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Localization;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Localization;

namespace HelloWorldConsole
{
  public class ConsoleApplication
  {
    IStringLocalizer localizer;

    public ConsoleApplication(IStringLocalizer localizer)
    {
      this.localizer = localizer;
    }

    public void Run()
    {
      Console.WriteLine("Hello World!");
    }
  }

  class Program
  {
    static void Main(string[] args)
    {
      var services = new ServiceCollection();
      services.AddTransient<ConsoleApplication>();
      //services.AddSingleton();

      var serviceProvider = services.BuildServiceProvider();
      serviceProvider.GetService<ConsoleApplication>().Run();

      services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

      var supportedCultures = new List<CultureInfo>
      {
        new CultureInfo("en"),
        new CultureInfo("fi"),
        new CultureInfo("de")
      };

      var options = new RequestLocalizationOptions
      {
        DefaultRequestCulture = new RequestCulture("en"),
        SupportedCultures = supportedCultures,
        SupportedUICultures = supportedCultures
      };
    }
  }
}
