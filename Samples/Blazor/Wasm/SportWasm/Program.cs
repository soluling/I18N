using System.Globalization;
using Microsoft.AspNetCore.Components.Web;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.JSInterop;
using Soluling.Sport;
using SportWasm;

var builder = WebAssemblyHostBuilder.CreateDefault(args);
builder.RootComponents.Add<App>("#app");
builder.RootComponents.Add<HeadOutlet>("head::after");

builder.Services.AddScoped(sp => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });
builder.Services.AddSingleton<SportService, SportService>();

// 1) Specify .resx directory.
builder.Services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

var host = builder.Build();

// Get the language. getLanguage returns the active language. It is the language of the browser.
var jsInterop = host.Services.GetRequiredService<IJSRuntime>();
var language = await jsInterop.InvokeAsync<string>("getLanguage");

// Set the default culture to match the language.
var culture = new CultureInfo(language);
CultureInfo.DefaultThreadCurrentCulture = culture;
CultureInfo.DefaultThreadCurrentUICulture = culture;

await host.RunAsync();
