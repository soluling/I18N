using Soluling.Sport;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddRazorPages();
builder.Services.AddServerSideBlazor();
builder.Services.AddSingleton<SportService, SportService>();

// 1) Specify .resx directory.
builder.Services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

var app = builder.Build();

// 2) Add supported languages: English and Finnish
var supportedCultures = new[] { "en", "fi" };

// 3) Configure application to use the above locales
var localizationOptions = new RequestLocalizationOptions()
  .SetDefaultCulture(supportedCultures[0])
  .AddSupportedCultures(supportedCultures)
  .AddSupportedUICultures(supportedCultures);

app.UseRequestLocalization(localizationOptions);

if (!app.Environment.IsDevelopment())
  app.UseExceptionHandler("/Error");

app.UseStaticFiles();
app.UseRouting();

app.MapBlazorHub();
app.MapFallbackToPage("/_Host");

app.Run();
