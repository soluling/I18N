using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Localization;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Options;

namespace Soluling.AspNet
{
  /// <summary>
  /// Extenstion methods for IApplicationBuilder
  /// </summary>
  public static class BuilderExtension
  {
    /// <summary>
    /// Configures request localization options to use those languages that has an existing satellite assembly.
    /// </summary>
    /// <param name="services">App's services.</param>
    /// <param name="assemblyFileName">Full path name of the main assembly file name.</param>
    /// <param name="defaultLanguage">Default language id.</param>
    public static void ConfigureWithAvailableLanguages(
      this IServiceCollection services,
      string assemblyFileName, 
      string defaultLanguage)
    {
      services.Configure<RequestLocalizationOptions>(
        opts =>
        {
          var supportedCultures = GetSupportedLanguages(assemblyFileName, defaultLanguage);

          opts.DefaultRequestCulture = new RequestCulture(defaultLanguage);
          opts.SupportedCultures = supportedCultures;
          opts.SupportedUICultures = supportedCultures;
        });
    }

    /// <summary>
    /// Configures request localization to use request localization options.
    /// </summary>
    /// <param name="app">Application builder to configure.</param>
    public static void UseRequestLocalizationWithConfiguredLanguages(this IApplicationBuilder app)
    {
      var options = app.ApplicationServices.GetService<IOptions<RequestLocalizationOptions>>();
      app.UseRequestLocalization(options.Value);
    }

    /// <summary>
    /// Configures request localization to use those languages that has an existing satellite assembly.
    /// </summary>
    /// <param name="app">Application builder to configure.</param>
    /// <param name="assemblyFileName">Full path name of the main assembly file name.</param>
    /// <param name="defaultLanguage">Default language id.</param>
    public static void UseRequestLocalizationWithAvailableLanguages(
      this IApplicationBuilder app, 
      string assemblyFileName, 
      string defaultLanguage)
    {
      var supportedCultures = GetSupportedLanguages(assemblyFileName, defaultLanguage);

      // Set the localization options for request localization
      var options = new RequestLocalizationOptions
      {
        DefaultRequestCulture = new RequestCulture(defaultLanguage),
        SupportedCultures = supportedCultures,
        SupportedUICultures = supportedCultures
      };

      app.UseRequestLocalization(options);
    }

    private static List<CultureInfo> GetSupportedLanguages(
      string assemblyFileName, 
      string defaultLanguage)
    {
      var supportedCultures = Language.GetAvailableLanguages(assemblyFileName).ToList();

      // Add default language (if not already)
      if (supportedCultures.Find(culture => culture.Name == defaultLanguage) == null)
        supportedCultures.Insert(0, new CultureInfo(defaultLanguage));

      return supportedCultures;
    }
  }
}
