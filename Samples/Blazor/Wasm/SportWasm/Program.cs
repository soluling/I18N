using System;
using System.Globalization;
using System.Net.Http;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.JSInterop;
using Soluling.Sport;

namespace SportWasm
{
  public class Program
  {
    public static async Task Main(string[] args)
    {
      var builder = WebAssemblyHostBuilder.CreateDefault(args);
      builder.RootComponents.Add<App>("app");
      
      builder.Services.AddSingleton(new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });
      builder.Services.AddSingleton<SportService, SportService>();

      // Set the folder that contains the resource files
      builder.Services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

      var host = builder.Build();

      // Get the language. blazorCulture.get return the active language.
      // By default it is the language of the browser. However, if the language has been save to the local storage
      // by calling blazorCulture.set then the fucntion returns the stored language.
      var jsInterop = host.Services.GetRequiredService<IJSRuntime>();
      var language = await jsInterop.InvokeAsync<string>("getLanguage");

      // Set the default culture to match the language.
      var culture = new CultureInfo(language);
      CultureInfo.DefaultThreadCurrentCulture = culture;
      CultureInfo.DefaultThreadCurrentUICulture = culture;

      await host.RunAsync();
    }
  }
}
