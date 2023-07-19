using System.Reflection;
using Microsoft.AspNetCore.Mvc.Razor;
using Soluling.AspNet;
using Soluling.Sport;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddScoped<SportService, SportService>();

// 1) Specify .resx directory.
builder.Services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

// 2) Add the view and data annotation localization.
builder.Services.AddRazorPages()
  .AddViewLocalization(LanguageViewLocationExpanderFormat.Suffix)
  .AddDataAnnotationsLocalization();

var app = builder.Build();

// 3) Add available languages and English as a default language
app.UseRequestLocalizationWithAvailableLanguages(Assembly.GetExecutingAssembly().Location, "en");

if (!app.Environment.IsDevelopment())
  app.UseExceptionHandler("/Error");

app.UseStaticFiles();
app.UseRouting();
app.UseAuthorization();
app.MapRazorPages();

app.Run();
