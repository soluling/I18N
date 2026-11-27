using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Resources;
using System.Runtime.Loader;
using System.Text.Json;
using System.Text.RegularExpressions;
using System.Xml.Linq;

namespace Soluling.AspNet
{
  /// <summary>
  /// The translations downloaded from Soluling Cloud: resource name -> culture -> string name ->
  /// text. An immutable snapshot, replaced as a whole when an update arrives, so a request never sees
  /// a half-updated set. Kept on disk too (see <see cref="OtaOptions.CacheDir"/>).
  /// </summary>
  public class OtaStore
  {
    private const string INDEX_FILE = "index.json";

    // resource name -> culture name -> string name -> text
    private volatile Dictionary<string, Dictionary<string, Dictionary<string, string>>> translations = new(StringComparer.OrdinalIgnoreCase);

    // localizer suffix -> the resource names it matched, so the suffix match runs once per localizer
    private volatile Dictionary<string, string[]> matches = new(StringComparer.OrdinalIgnoreCase);

    private readonly object updateLock = new();

    /// <summary>The published files kept now: file name -> (hash, language, local file).</summary>
    internal Dictionary<string, OtaFile> Files { get; private set; } = new(StringComparer.OrdinalIgnoreCase);

    internal string CacheDir { get; }

    /// <summary>Creates the store and reads the translations kept on disk.</summary>
    public OtaStore(OtaOptions options)
    {
      CacheDir = options.GetCacheDir();
      LoadFromDisk();
    }

    /// <summary>Whether there are any downloaded translations.</summary>
    public bool IsEmpty => translations.Count == 0;

    /// <summary>
    /// The downloaded text of a string, looking at the culture and then its parents (fi-FI, fi), or
    /// null when there is none.
    /// </summary>
    /// <param name="resourceSuffix">The end of the resource name, such as Controllers.HomeController.</param>
    /// <param name="culture">The culture, normally CultureInfo.CurrentUICulture.</param>
    /// <param name="name">The name of the string.</param>
    public string Find(string resourceSuffix, CultureInfo culture, string name)
    {
      var current = translations;
      var resources = FindResources(resourceSuffix);

      for (var c = culture; (c != null) && (c != CultureInfo.InvariantCulture); c = c.Parent)
      {
        foreach (var resource in resources)
        {
          if (current.TryGetValue(resource, out var cultures) && cultures.TryGetValue(c.Name, out var strings) && strings.TryGetValue(name, out var value))
            return value;
        }
      }

      return null;
    }

    /// <summary>All downloaded strings of a resource in a culture, the parents' too when includeParentCultures.</summary>
    public IReadOnlyDictionary<string, string> GetAll(string resourceSuffix, CultureInfo culture, bool includeParentCultures)
    {
      var result = new Dictionary<string, string>();
      var current = translations;
      var resources = FindResources(resourceSuffix);

      for (var c = culture; (c != null) && (c != CultureInfo.InvariantCulture); c = c.Parent)
      {
        foreach (var resource in resources)
        {
          if (current.TryGetValue(resource, out var cultures) && cultures.TryGetValue(c.Name, out var strings))
          {
            foreach (var pair in strings)
              result.TryAdd(pair.Key, pair.Value);
          }
        }

        if (!includeParentCultures)
          break;
      }

      return result;
    }

    // The resources whose name ends with the suffix at a dot - Resources.Controllers.HomeController
    // for Controllers.HomeController. Several can: a .resx is named by its path and a satellite by
    // its embedded name, so the files of different languages can have different resource names for
    // the same resource. The shortest first, as the closest match.
    private string[] FindResources(string suffix)
    {
      if (string.IsNullOrEmpty(suffix))
        return Array.Empty<string>();

      var current = matches;

      if (current.TryGetValue(suffix, out var found))
        return found;

      found = translations.Keys
        .Where(key => key.Equals(suffix, StringComparison.OrdinalIgnoreCase) || key.EndsWith("." + suffix, StringComparison.OrdinalIgnoreCase))
        .OrderBy(key => key.Length)
        .ToArray();

      matches = new Dictionary<string, string[]>(current, StringComparer.OrdinalIgnoreCase) { [suffix] = found };
      return found;
    }

    /// <summary>
    /// Replaces the translations with the files given: each published file name with its hash,
    /// language, and local copy. Reads them, swaps the snapshot, and saves the index.
    /// </summary>
    internal void Replace(Dictionary<string, OtaFile> files)
    {
      lock (updateLock)
      {
        var result = new Dictionary<string, Dictionary<string, Dictionary<string, string>>>(StringComparer.OrdinalIgnoreCase);

        foreach (var file in files.Values)
        {
          try
          {
            foreach (var (resource, strings) in Read(file))
            {
              if (!result.TryGetValue(resource, out var cultures))
                result[resource] = cultures = new Dictionary<string, Dictionary<string, string>>(StringComparer.OrdinalIgnoreCase);

              cultures[NormalizeCulture(file.Language)] = strings;
            }
          }
          catch
          {
            // A file that can't be read is left out - the built-in translations are used for it
          }
        }

        translations = result;
        matches = new Dictionary<string, string[]>(StringComparer.OrdinalIgnoreCase);
        Files = files;
        SaveIndex();
      }
    }

    private static string NormalizeCulture(string language)
    {
      try
      {
        return CultureInfo.GetCultureInfo(language).Name;
      }
      catch (CultureNotFoundException)
      {
        return language;
      }
    }

    // The resources of a downloaded file: a .resx is one resource, a satellite assembly one per .resources stream
    private static IEnumerable<(string Resource, Dictionary<string, string> Strings)> Read(OtaFile file)
    {
      if (file.FileName.EndsWith(".resx", StringComparison.OrdinalIgnoreCase))
        return new[] { (ResxResourceName(file.FileName, file.Language), ReadResx(file.LocalPath)) };

      if (file.FileName.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
        return ReadSatellite(file.LocalPath, file.Language);

      return Enumerable.Empty<(string, Dictionary<string, string>)>();
    }

    /// <summary>
    /// The resource name of a published .resx file: its path without the culture and the extension,
    /// with dots for folders. src/MyApp/Resources/Controllers/HomeController.fi.resx is
    /// src.MyApp.Resources.Controllers.HomeController, which ends with the localizer's
    /// Controllers.HomeController.
    /// </summary>
    internal static string ResxResourceName(string fileName, string language)
    {
      var name = fileName.Replace('\\', '/');
      name = name.Substring(0, name.Length - ".resx".Length);

      if (!string.IsNullOrEmpty(language) && name.EndsWith("." + language, StringComparison.OrdinalIgnoreCase))
        name = name.Substring(0, name.Length - language.Length - 1);

      return name.Trim('/').Replace('/', '.');
    }

    // The string values of a .resx: data elements with no type (or a string type) and no mimetype
    internal static Dictionary<string, string> ReadResx(string path)
    {
      var result = new Dictionary<string, string>();
      var document = XDocument.Load(path);

      foreach (var data in document.Root?.Elements("data") ?? Enumerable.Empty<XElement>())
      {
        var name = (string)data.Attribute("name");
        var type = (string)data.Attribute("type");

        if (string.IsNullOrEmpty(name) || (data.Attribute("mimetype") != null))
          continue;

        if (!string.IsNullOrEmpty(type) && !type.StartsWith("System.String", StringComparison.Ordinal))
          continue;

        var value = data.Element("value");

        if (value != null)
          result[name] = value.Value;
      }

      return result;
    }

    // The strings of a satellite assembly, read as data in a context that is unloaded afterwards, so
    // the file is neither locked nor kept in memory. MyApp.Resources.HomeController.fi.resources is
    // the resource MyApp.Resources.HomeController.
    private static IEnumerable<(string, Dictionary<string, string>)> ReadSatellite(string path, string language)
    {
      var result = new List<(string, Dictionary<string, string>)>();
      var context = new AssemblyLoadContext("SolulingOta", isCollectible: true);

      try
      {
        using var stream = new MemoryStream(File.ReadAllBytes(path));
        var assembly = context.LoadFromStream(stream);

        foreach (var name in assembly.GetManifestResourceNames().Where(n => n.EndsWith(".resources", StringComparison.OrdinalIgnoreCase)))
        {
          using var resourceStream = assembly.GetManifestResourceStream(name);

          if (resourceStream == null)
            continue;

          var strings = new Dictionary<string, string>();

          using (var reader = new ResourceReader(resourceStream))
          {
            var enumerator = reader.GetEnumerator();

            while (enumerator.MoveNext())
            {
              if (enumerator.Value is string value)
                strings[(string)enumerator.Key] = value;
            }
          }

          var resource = name.Substring(0, name.Length - ".resources".Length);

          if (!string.IsNullOrEmpty(language) && resource.EndsWith("." + language, StringComparison.OrdinalIgnoreCase))
            resource = resource.Substring(0, resource.Length - language.Length - 1);

          result.Add((resource, strings));
        }
      }
      finally
      {
        context.Unload();
      }

      return result;
    }

    private void LoadFromDisk()
    {
      try
      {
        var index = Path.Combine(CacheDir, INDEX_FILE);

        if (string.IsNullOrEmpty(CacheDir) || !File.Exists(index))
          return;

        var files = JsonSerializer.Deserialize<List<OtaFile>>(File.ReadAllText(index)) ?? new List<OtaFile>();

        Replace(files
          .Where(file => File.Exists(file.LocalPath))
          .ToDictionary(file => file.FileName, StringComparer.OrdinalIgnoreCase));
      }
      catch
      {
        // A damaged cache only means waiting for the first check
      }
    }

    private void SaveIndex()
    {
      if (string.IsNullOrEmpty(CacheDir))
        return;

      try
      {
        Directory.CreateDirectory(CacheDir);
        File.WriteAllText(Path.Combine(CacheDir, INDEX_FILE), JsonSerializer.Serialize(Files.Values.ToList()));
      }
      catch
      {
        // Not saved: the translations still work until a restart
      }
    }

    private static readonly Regex PlaceholderRegex = new(@"\{(\d+)[^{}]*\}", RegexOptions.Compiled);

    /// <summary>
    /// Whether two texts have the same composite format placeholders ({0}, {1:N2}...), in any order.
    /// A translation that differs would throw in string.Format, or show wrong values.
    /// </summary>
    public static bool SamePlaceholders(string original, string translation)
    {
      static List<string> Placeholders(string text) =>
        PlaceholderRegex.Matches((text ?? "").Replace("{{", "").Replace("}}", ""))
          .Select(match => match.Groups[1].Value)
          .Distinct()
          .OrderBy(index => index)
          .ToList();

      return Placeholders(original).SequenceEqual(Placeholders(translation));
    }
  }

  /// <summary>A published file kept by the store.</summary>
  internal class OtaFile
  {
    public string FileName { get; set; }
    public string Language { get; set; }
    public string Hash { get; set; }
    public string LocalPath { get; set; }
  }
}
