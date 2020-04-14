using System;
using System.Collections.Generic;
using System.Globalization;
using System.Net.Http;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.Extensions.DependencyInjection;

namespace BlazorSportWasm
{
  public class Program
  {
    public static async Task Main(string[] args)
    {
      var builder = WebAssemblyHostBuilder.CreateDefault(args);
      builder.RootComponents.Add<App>("app");
      
      //builder.Services.AddSingleton(new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });
      builder.Services.AddBaseAddressHttpClient();

      // I18N
      builder.Services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

/*
      var supportedCultures = new[]
      {
        new CultureInfo("en"),
        new CultureInfo("fi"),
      };
*/

      var culture = new CultureInfo("fi");
      CultureInfo.DefaultThreadCurrentCulture = culture;    
      CultureInfo.DefaultThreadCurrentUICulture = culture;

      await builder.Build().RunAsync();
    }
  }
}
