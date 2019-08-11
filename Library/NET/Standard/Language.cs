using System;
using System.Globalization;
using System.Collections;
using System.IO;
using System.Reflection;
using System.Resources;
using System.Threading;

namespace Soluling
{
  /// <summary>
  /// Specifies how language name is shown.
  /// </summary>
  public enum LanguageName 
  { 
    /// <summary>Both native and English names are shown.</summary>
    Both, 
    /// <summary>Native name is shown.</summary>
    Native, 
    /// <summary>English name is shown.</summary>
    English 
  };

  /// <summary>
  /// Implements current locale and resource cultures. Provides methods to get list of available languages for the application. Also provides methods to get resource data that has been added as Resource or Embedded Resource.
  /// </summary>
  /// <remarks>
  /// <para>
  /// If you want to create a custom user interface to show available languages for the application use this class to the list of the available languages for the application.
  /// </para>
  /// <para>
  /// ResourceManager performs language cascading where the resource is first looked from the resource of the specific culture. If not found them the search is 
  /// continue from the parent culture all the way to neutral culture (e.g. the resource in the original assembly file). 
  /// Unfortunately ResourceManager only works with resources that are in the .resx file. If you have added a file as Resource or Embedded Resource into 
  /// your project ResourceManager cannot access them. In that case use this class.
  /// </para>
  /// </remarks>
  /// <example>
  /// <para>
  /// The following gets the list of available languages for the application and uses it to populate the language selection user interface.
  /// </para>
  /// <code>
  /// var languages Resources.GetAvailableLanguages();
  /// 
  /// foreach (CultureInfo language in languages)
  /// {
  ///   AddLanguage(language.ToString());
  /// }
  /// </code>
  /// <para>
  /// The following example loads a standard resource and an embedded resource into picture boxes.
  /// </para>
  /// <code>
  /// Soluling.Language.SetInitial();
  ///
  /// resourcePictureBox.Image = new Bitmap(Resources.GetStandardResource("airplane.png"));
  /// embeddedResourcePictureBox.Image = new Bitmap(Resources.GetEmbeddedResource("car_sedan_blue.png"));
  /// </code>
  /// </example>
  public class Language
  {
    private static Assembly activeAssembly = null;
    private static Assembly mainAssembly = null;
    private static string assemblyName = "";
    private static CultureInfo culture;

    /// <summary>
    /// Get and set the current culture.
    /// </summary>
    /// <seealso cref="P:Soluling.Language.Id"/>
    public static CultureInfo Culture
    {
      get
      {
        if (culture != null)
          return culture;
        else
          return AssemblyCulture;
      }
      set
      {
        culture = value;

        if (value != null)
          SetInitial(value);
      }
    }

    /// <summary>
    /// Get and set the current locale id.
    /// </summary>
    /// <seealso cref="P:Soluling.Language.Culture"/>
    public static string Id
    {
      get { return Culture.ToString(); }
      set { Culture = new CultureInfo(value); }
    }

    /// <summary>
    /// Get the locale id of the currently active satellite assembly.
    /// </summary>
    /// <seealso cref="P:Soluling.Language.AssemblyCulture"/>
    public static string AssemblyId
    {
      get
      {
        CheckActiveAssembly();

        if (activeAssembly != mainAssembly)
          return new AssemblyName(activeAssembly.FullName).CultureInfo.ToString();
        else
          return OriginalId;
      }
    }

    /// <summary>
    /// Get the culture of the currently active satellite assembly.
    /// </summary>
    /// <seealso cref="P:Soluling.Language.AssemblyId"/>
    public static CultureInfo AssemblyCulture
    {
      get
      {
        return new CultureInfo(AssemblyId);
      }
    }

    /// <summary>
    /// Get the locale id of the original assembly.
    /// </summary>
    /// <remarks>
    /// Original language is read from the value NeutralResourcesLanguage attribute. If there are no NeutralResourcesLanguage attribute then
    /// English (en) is returned.
    /// </remarks>
    /// <seealso cref="P:Soluling.Language.OriginalCulture"/>
    public static string OriginalId
    {
      get
      {
        var assembly = Assembly.GetEntryAssembly();

        if (assembly == null)
          return "en";

        var attributes = assembly.GetCustomAttributes(typeof(NeutralResourcesLanguageAttribute), false);

        if (attributes.Length > 0)
          return ((NeutralResourcesLanguageAttribute)attributes[0]).CultureName;
        else
          return "en";
      }
    }

    /// <summary>
    /// Get the original culture.
    /// </summary>
    /// <seealso cref="P:Soluling.Language.OriginalId"/>
    public static CultureInfo OriginalCulture
    {
      get
      {
        return new CultureInfo(OriginalId);
      }
    }

    /// <summary>
    /// Set the initial culture and UI culture.
    /// </summary>
    /// <param name="culture">Locale id. If null use the default locale set in Control Panel.</param>
    /// <seealso cref="M:Soluling.Language.SetInitial(System.String)"/>
    public static void SetInitial(CultureInfo culture = null)
    {
      if (culture == null)
        culture = CultureInfo.CurrentCulture;

      Thread.CurrentThread.CurrentUICulture = culture;
      Thread.CurrentThread.CurrentCulture = culture;
    }

    /// <summary>
    /// Set the initial culture and UI culture to a language.
    /// </summary>
    /// <param name="id">Locale id.</param>
    /// <seealso cref="M:Soluling.Language.SetInitial(System.Globalization.CultureInfo)"/>
    public static void SetInitial(string id)
    {
      CultureInfo culture;

      if (string.IsNullOrEmpty(id))
        culture = CultureInfo.CurrentCulture;
      else
        culture = new CultureInfo(id);

      Thread.CurrentThread.CurrentUICulture = culture;
      Thread.CurrentThread.CurrentCulture = culture;
    }

    private static void CheckActiveAssembly()
    {
      if (mainAssembly != null)
        return;

      mainAssembly = Assembly.GetEntryAssembly();

      if (mainAssembly == null)
        return;

      assemblyName = GetAssemblyName(mainAssembly);

      // Get the active culture. Get satellite assembly files matching the current culture. 
      // If not found try to get the assembly of the parent culture and so on.
      activeAssembly = null;
      CultureInfo cultureInfo = Thread.CurrentThread.CurrentUICulture;

      while ((activeAssembly == null) && (cultureInfo != null) && (cultureInfo.Name != ""))
      {
        try
        {
          activeAssembly = mainAssembly.GetSatelliteAssembly(cultureInfo);
        }
        catch (Exception)
        {
        }

        if (activeAssembly != null)
          break;

        cultureInfo = cultureInfo.Parent;
      }

      // If no file found use the main assembly
      if (activeAssembly == null)
        activeAssembly = mainAssembly;
    }

    /// <summary>
    /// Gets the name of the assembly.
    /// </summary>
    /// <param name="assembly">Assembly object.</param>
    /// <returns>Assembly name.</returns>
    public static string GetAssemblyName(Assembly assembly)
    {
      if (assembly == null)
        return "";

      string result = assembly.FullName;

      int p = result.IndexOf(',');

      if (p >= 0)
        result = result.Substring(0, p);

      return result;
    }

    /// <summary>
    /// Get the culture of the resource item name.
    /// </summary>
    /// <param name="baseName">Resource file name.</param>
    /// <param name="assembly">Assembly that contains the resource file.</param>
    /// <param name="resourceName">Specifies the resource item name.</param>
    /// <param name="culture">Culture that is used as the start culture.</param>
    /// <returns>Culture info of the resource item. null if there are no localized value of the resource but only the original language independent resource.</returns>
    public static CultureInfo GetResourceCulture(string baseName, Assembly assembly, string resourceName, CultureInfo culture = null)
    {
      return GetResourceCulture(new ResourceManager(baseName, assembly), resourceName, culture);
    }

    /// <summary>
    /// Get the culture of the resource item name.
    /// </summary>
    /// <param name="manager">Resource manager that is used to find resources.</param>
    /// <param name="resourceName">Specifies the resource item name.</param>
    /// <param name="culture">Culture that is used as the start culture.</param>
    /// <returns>Culture info of the resource item. null if there are no localized value of the resource but only the original language independent resource.</returns>
    public static CultureInfo GetResourceCulture(ResourceManager manager, string resourceName, CultureInfo culture = null)
    {
      return CultureInfo.CurrentUICulture;
/*
  #if !NETFX_CORE
      manager.ReleaseAllResources();

      if (culture == null)
        culture = Thread.CurrentThread.CurrentUICulture;

      while (culture != null)
      {
        ResourceSet resourceSet = manager.GetResourceSet(culture, true, false);

        if (resourceSet != null)
        {
          Object item = resourceSet.GetObject(resourceName);

          if ((item != null) && (item is String))
            return culture;
        }

        if (culture.Name == "")
          break;
        else
          culture = culture.Parent;
      }
  #endif

      return null;
*/
    }

    private static string GetResourceName(string resourceName)
    {
      return assemblyName + '.' + resourceName;
    }

    private static string GetIdResourceName(string resourceName, string id)
    {
      int p = resourceName.LastIndexOf('.');

      if (p >= 0)
        resourceName = resourceName.Substring(0, p + 1) + id + resourceName.Substring(p, resourceName.Length - p);

      return GetResourceName(resourceName);
    }

    private static string GetCultureResourceName(string resourceName)
    {
      return GetIdResourceName(resourceName, Language.Id);
    }

    private static string GetLanguageResourceName(string resourceName)
    {
      return GetIdResourceName(resourceName, Language.Culture.TwoLetterISOLanguageName);
    }

    /// <summary>
    /// Get a stream for an embedded resource.
    /// </summary>
    /// <param name="resourceName">Name of the resource</param>
    /// <returns>Stream for the resource</returns>
    public static Stream GetEmbeddedResource(string resourceName)
    {
      CheckActiveAssembly();

      if (activeAssembly != mainAssembly)
      {
        CultureInfo culture = new AssemblyName(activeAssembly.FullName).CultureInfo;

        // First try culture specific resource name in the satellite assembly file
        // de\Application.resources.dll: Application.Sample.de.png
        // de-DE\Application.resources.dll: Application.Sample.de-DE.png
        Stream value = activeAssembly.GetManifestResourceStream(GetIdResourceName(resourceName, culture.ToString()));

        if (value != null)
          return value;

        // Try language only specific resource name in the satellite assembly file
        // de-DE\Application.resources.dll: Application.Sample.de.png
        if (culture.ToString() != culture.TwoLetterISOLanguageName)
        {
          value = activeAssembly.GetManifestResourceStream(GetIdResourceName(resourceName, culture.TwoLetterISOLanguageName));

          if (value != null)
            return value;
        }

        // Try the original resource name in the satellite assembly file
        // de\MyApplication.resources.dll: Application.Sample.png
        // de-DE\MyApplication.resources.dll: Application.Sample.png
        value = activeAssembly.GetManifestResourceStream(GetResourceName(resourceName));

        if (value != null)
          return value;

        // Try original resource
        // Application.exe: Application.Sample.png
        if (mainAssembly != null)
          return mainAssembly.GetManifestResourceStream(GetResourceName(resourceName));
        else
          return null;
      }
      else
      {
        // No satellite assembly file
        // Application.exe: Application.Sample.png
        return mainAssembly.GetManifestResourceStream(GetResourceName(resourceName));
      }
    }
  
    private static Stream GetStandardResource(Stream resourceStream, string resourceName)
    {
      using (ResourceReader resourceReader = new ResourceReader(resourceStream))
      {
        foreach (DictionaryEntry resourceEntry in resourceReader)
          if (resourceEntry.Key.ToString() == resourceName)
            return resourceEntry.Value as Stream;
      }

      return null;
    }

    /// <summary>
    /// Get a stream for a standard resource.
    /// </summary>
    /// <param name="resourceName">Name of the resource</param>
    /// <returns>Stream for the resource</returns>
    public static Stream GetStandardResource(string resourceName)
    {
      resourceName = resourceName.ToLower();

      Stream result = GetStandardResource(GetEmbeddedResource("g.resources"), resourceName);

      if (result != null)
        return result;

      if (mainAssembly != activeAssembly)
        return GetStandardResource(
          mainAssembly.GetManifestResourceStream(GetResourceName("g.resources")), 
          resourceName);
      else
        return null;
    }

    /// <summary>
    /// Gets the display name of the language.
    /// </summary>
    /// <param name="cultureInfo">The culture info.</param>
    /// <param name="languageName">The language name type.</param>
    /// <returns>The display name.</returns>
    public static string GetLanguageDisplayName(CultureInfo cultureInfo, LanguageName languageName)
    {
      switch (languageName)
      {
        case LanguageName.English:
          return cultureInfo.EnglishName;

        case LanguageName.Native:
          return cultureInfo.NativeName;

        default:
          return cultureInfo.NativeName + " - " + cultureInfo.EnglishName;
      }
    }

    private static string GetCultureFileName(string fileName)
    {
      // Sample.png -> de\Sample.png
      //            -> de-DE\Sample.png
      string dir = Path.GetDirectoryName(fileName);

      if (dir == "")
        dir = Path.GetDirectoryName(mainAssembly.Location);

      return dir + "\\" + Language.Id + "\\" + Path.GetFileName(fileName);
    }

    /// <summary>
    /// Get a stream for a file.
    /// </summary>
    /// <param name="fileName">Neutral filename</param>
    /// <returns>Stream for the localized file</returns>
    public static Stream GetFileResource(string fileName)
    {
      string thisFileName = GetCultureFileName(fileName);

      if (!File.Exists(thisFileName))
        thisFileName = fileName;

      return new FileStream(thisFileName, FileMode.Open, FileAccess.Read);
    }

    /// <summary>
    /// Check if there exists any satellite assembly files.
    /// </summary>
    public static void CheckSatelliteAssemblyCount()
    {
      if (GetAvailableLanguages().Length == 0)
        throw new Exception("In order to change language you must first create satellite assembly files for the application.");
    }

    /// <summary>
    /// Get array of available languages for the current application.
    /// </summary>
    /// <remarks>
    /// Use this method to get list of available language if you want to build your own user interface to show available languages and to select a new language.
    /// You can also use the built-in language selection dialogs. If you use Windows Forms use the <see cref="T:Soluling.Forms.SelectLanguage"/> class. 
    /// If you use WPF use the <see cref="T:Soluling.WPF.SelectLanguage"/> class.
    /// </remarks>
    /// <returns>Array of culture infos of the satellite assemblies.</returns>
    public static CultureInfo[] GetAvailableLanguages()
    {
      return GetAvailableLanguages(Assembly.GetExecutingAssembly().Location);
    }

    /// <summary>
    /// Get array of available languages for the specified application.
    /// </summary>
    /// <param name="fileName">Filename of the assembly whose satellite assemblies are listed</param>
    /// <returns>Array of culture infos of the satellite assemblies.</returns>
    public static CultureInfo[] GetAvailableLanguages(string fileName)
    {
      const string SATELLITE_EXT = ".resources.dll";

      string dir = new FileInfo(fileName).Directory.FullName;
      string satelliteFileName = Path.GetFileNameWithoutExtension(fileName) + SATELLITE_EXT;
      DirectoryInfo di = new DirectoryInfo(dir);
      DirectoryInfo[] dirs = di.GetDirectories();
      CultureInfo[] allCultures = CultureInfo.GetCultures(CultureTypes.AllCultures);
      ArrayList cultureArray = new ArrayList();

      for (int i = 0; i < dirs.Length; i++)
      {
        di = dirs[i];
        string str = di.Name;
        bool found = false;

        for (int j = 0; j < allCultures.Length; j++)
        {
          if ((allCultures[j].Name == str) || (allCultures[j].TwoLetterISOLanguageName == str))
          {
            FileInfo[] files = di.GetFiles();

            for (int k = 0; k < files.Length; k++)
            {
              FileInfo fileInfo = files[k];

              if (fileInfo.Name.IndexOf(SATELLITE_EXT) > 0)
              {
                found = true;
                break;
              }
            }

            if (found)
              break;
          }
        }

        if (found)
          cultureArray.Add(new CultureInfo(str));
      }

      CultureInfo[] resultCultures = new CultureInfo[cultureArray.Count];

      for (int i = 0; i < cultureArray.Count; i++)
        resultCultures[i] = cultureArray[i] as CultureInfo;

      return resultCultures;
    }
  }
}
