var builder = WebApplication.CreateBuilder(args);

builder.Services
  .AddControllers()
  .AddNewtonsoftJson(options => options.SerializerSettings.Formatting = Newtonsoft.Json.Formatting.Indented);

builder.Services.AddEndpointsApiExplorer();

// 1) Specify .resx directory.
builder.Services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

var app = builder.Build();

// 2) Add supported languages: Finnish and German
var supportedCultures = new[] { "en", "fi" };

// 3) Configure application to use the above locales
var options = new RequestLocalizationOptions()
  .SetDefaultCulture(supportedCultures[0])
  .AddSupportedCultures(supportedCultures)
  .AddSupportedUICultures(supportedCultures);

app.UseRequestLocalization(options);

app.UseAuthorization();
app.MapControllers();

app.Run();
