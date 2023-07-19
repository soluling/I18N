var builder = WebApplication.CreateBuilder(args);
builder.Services.AddControllersWithViews();

// 1) Specify .resx directory.
builder.Services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

var app = builder.Build();

// 2) Add supported languages: English, Finnish and German
var supportedCultures = new[] { "en", "fi", "de" };

// 3) Configure application to use the above locales
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
