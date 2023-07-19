using Microsoft.AspNetCore.Mvc.Razor;

var builder = WebApplication.CreateBuilder(args);

// 1) Specify .resx directory. Add Resources directory and add Controllers.DefaultController.resx file.
builder.Services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

// 2) Add the view and data annotation localization.
builder.Services.AddControllersWithViews()
  .AddViewLocalization(LanguageViewLocationExpanderFormat.Suffix)
  .AddDataAnnotationsLocalization();

var app = builder.Build();

// 3) Add supported languages: Finnish and German
var supportedCultures = new[] { "en", "fi", "de" };

// 4) Configure application to use the above locales
var options = new RequestLocalizationOptions()
  .SetDefaultCulture(supportedCultures[0])
  .AddSupportedCultures(supportedCultures)
  .AddSupportedUICultures(supportedCultures);

app.UseRequestLocalization(options);

if (!app.Environment.IsDevelopment())
  app.UseExceptionHandler("/Home/Error");

app.UseStaticFiles();
app.UseRouting();
app.UseAuthorization();

app.MapControllerRoute(
    name: "default",
    pattern: "{controller=Default}/{action=Index}/{id?}");

app.Run();
