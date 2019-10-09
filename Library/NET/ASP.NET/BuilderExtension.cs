using System.Globalization;
using System.Linq;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Localization;

namespace Soluling.AspNet
{
  /// <summary>
  /// Extenstion methods for IApplicationBuilder
  /// </summary>
  public static class BuilderExtension
  {
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
      var supportedCultures = Language.GetAvailableLanguages(assemblyFileName).ToList();

      // Add default language (if not already)
      if (supportedCultures.Find(culture => culture.Name == defaultLanguage) == null)
        supportedCultures.Insert(0, new CultureInfo(defaultLanguage));

      // Set the localization options for request localization
      var options = new RequestLocalizationOptions
      {
        DefaultRequestCulture = new RequestCulture(defaultLanguage),
        SupportedCultures = supportedCultures,
        SupportedUICultures = supportedCultures
      };

      app.UseRequestLocalization(options);
    }
  }
}
