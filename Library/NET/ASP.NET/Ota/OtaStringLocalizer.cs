using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using Microsoft.Extensions.Localization;

namespace Soluling.AspNet
{
  /// <summary>
  /// Creates localizers that use the translations downloaded from Soluling Cloud first and the
  /// built-in ones (normally the satellite assemblies) otherwise - see
  /// <see cref="OtaServiceCollectionExtension.AddSolulingOta"/>. Wraps the factory that was
  /// registered before, so IStringLocalizer&lt;T&gt;, IViewLocalizer, IHtmlLocalizer and data annotation
  /// messages all get the downloaded translations.
  /// </summary>
  public class OtaStringLocalizerFactory : IStringLocalizerFactory
  {
    private readonly IStringLocalizerFactory inner;
    private readonly OtaStore store;

    /// <summary>Creates the factory.</summary>
    public OtaStringLocalizerFactory(IStringLocalizerFactory inner, OtaStore store)
    {
      this.inner = inner;
      this.store = store;
    }

    /// <inheritdoc/>
    public IStringLocalizer Create(Type resourceSource)
    {
      return new OtaStringLocalizer(inner.Create(resourceSource), store, GetSuffix(resourceSource));
    }

    /// <inheritdoc/>
    public IStringLocalizer Create(string baseName, string location)
    {
      return new OtaStringLocalizer(inner.Create(baseName, location), store, GetSuffix(baseName, location));
    }

    /// <summary>
    /// The end of the resource name of a type: its full name without the root namespace -
    /// Controllers.HomeController for MyApp.Controllers.HomeController - the part that the resource
    /// file's path ends with in every resources folder layout.
    /// </summary>
    internal static string GetSuffix(Type type)
    {
      var name = (type.FullName ?? type.Name).Replace('+', '.');
      var root = type.Assembly.GetCustomAttribute<RootNamespaceAttribute>()?.RootNamespace ?? type.Assembly.GetName().Name;

      return RemovePrefix(name, root);
    }

    /// <summary>The end of a base name: without the location (the application's name) as its prefix.</summary>
    internal static string GetSuffix(string baseName, string location)
    {
      return RemovePrefix(baseName ?? "", location);
    }

    private static string RemovePrefix(string name, string prefix)
    {
      if (!string.IsNullOrEmpty(prefix) && name.StartsWith(prefix + ".", StringComparison.Ordinal))
        return name.Substring(prefix.Length + 1);

      return name;
    }
  }

  /// <summary>
  /// A localizer that looks at the translations downloaded from Soluling Cloud first, and uses the
  /// built-in localizer for the strings they don't have. A downloaded text whose placeholders differ
  /// from the original's is not used, so it can't make string.Format throw.
  /// </summary>
  public class OtaStringLocalizer : IStringLocalizer
  {
    private readonly IStringLocalizer inner;
    private readonly OtaStore store;
    private readonly string suffix;

    /// <summary>Creates the localizer.</summary>
    public OtaStringLocalizer(IStringLocalizer inner, OtaStore store, string suffix)
    {
      this.inner = inner;
      this.store = store;
      this.suffix = suffix;
    }

    /// <inheritdoc/>
    public LocalizedString this[string name]
    {
      get
      {
        var value = store.Find(suffix, CultureInfo.CurrentUICulture, name);

        if (value == null)
          return inner[name];

        // The original's placeholders - a text with others would break the composite format later
        var original = inner[name];

        if (!OtaStore.SamePlaceholders(original.Value, value))
          return original;

        return new LocalizedString(name, value, false, "Soluling OTA");
      }
    }

    /// <inheritdoc/>
    public LocalizedString this[string name, params object[] arguments]
    {
      get
      {
        var value = store.Find(suffix, CultureInfo.CurrentUICulture, name);

        if (value == null)
          return inner[name, arguments];

        var original = inner[name];

        if (OtaStore.SamePlaceholders(original.Value, value))
        {
          try
          {
            return new LocalizedString(name, string.Format(CultureInfo.CurrentCulture, value, arguments), false, "Soluling OTA");
          }
          catch (FormatException)
          {
            // A broken format: the built-in text instead
          }
        }

        return inner[name, arguments];
      }
    }

    /// <inheritdoc/>
    public IEnumerable<LocalizedString> GetAllStrings(bool includeParentCultures)
    {
      var downloaded = store.GetAll(suffix, CultureInfo.CurrentUICulture, includeParentCultures);
      var names = new HashSet<string>();

      foreach (var builtIn in inner.GetAllStrings(includeParentCultures))
      {
        names.Add(builtIn.Name);

        if (downloaded.TryGetValue(builtIn.Name, out var value) && OtaStore.SamePlaceholders(builtIn.Value, value))
          yield return new LocalizedString(builtIn.Name, value, false, "Soluling OTA");
        else
          yield return builtIn;
      }

      // Strings only the download has: added in Soluling after the application was built
      foreach (var pair in downloaded.Where(pair => !names.Contains(pair.Key)))
        yield return new LocalizedString(pair.Key, pair.Value, false, "Soluling OTA");
    }
  }
}
