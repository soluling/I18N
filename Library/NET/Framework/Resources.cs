using System;
using System.Collections;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Resources;
using System.Threading;

namespace NewTool
{
  /// <summary>
  /// Specifies the language name type.
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
  /// Provides methods to get list of available languages for the application. Also provides methods to get resource data that has been added as Resource or Embedded Resource.
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
  /// Resources.Initialize(this.GetType().Assembly);
  ///
  /// resourcePictureBox.Image = new Bitmap(Resources.GetStandardResource("airplane.png"));
  /// embeddedResourcePictureBox.Image = new Bitmap(Resources.GetEmbeddedResource("car_sedan_blue.png"));
  /// </code>
  /// </example>
  public class Resources
  {
    static private Assembly activeAssembly = null;
    static private Assembly mainAssembly = null;
    static private string assemblyName = "";
    static private string culture = "";
    static private string originalCulture = "en";

    static private string GetResourceName(string resourceName)
    {
      return assemblyName + '.' + resourceName;
    }

    static private string GetCultureResourceName(string resourceName)
    {
      int p = resourceName.LastIndexOf('.');

      if (p >= 0)
        resourceName = resourceName.Substring(0, p + 1) + culture + resourceName.Substring(p, resourceName.Length - p);

      return GetResourceName(resourceName);
    }

    /// <summary>
    /// Gets the display name of the language.
    /// </summary>
    /// <param name="cultureInfo">The culture info.</param>
    /// <param name="languageName">The language name type.</param>
    /// <returns>The display name.</returns>
    static public string GetLanguageDisplayName(CultureInfo cultureInfo, LanguageName languageName)
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

    static private string GetCultureFileName(string fileName)
    {
      // Sample.png -> de\Sample.png
      //            -> de-DE\Sample.png
      string dir = Path.GetDirectoryName(fileName);

      if (dir == "")
        dir = Path.GetDirectoryName(mainAssembly.Location);

      return dir + "\\" + culture + "\\" + Path.GetFileName(fileName);
    }

    /// <summary>
    /// Gets the active language.
    /// </summary>
    /// <returns>Active language.</returns>
    static public string Language
    {
      get 
      {
        if (culture != "")
          return culture;
        else
          return originalCulture;
      }
    }

    /// <summary>
    /// Gets the original culture.
    /// </summary>
    /// <returns>Original culture.</returns>
    static public string OriginalLanguage
    {
      get { return originalCulture; }
    }

    /// <summary>
    /// Sets the culture.
    /// </summary>
    /// <param name="culture">The culture info to be set.</param>
    static public void SetCulture(CultureInfo culture)
    {
      Thread.CurrentThread.CurrentUICulture = culture;
      Resources.culture = culture.Name;
    }

    /// <summary>
    /// Set the original and default languages of the application.
    /// </summary>
    /// <param name="originalCulture">The original language of the application.</param>
    /// <param name="setDefaultLanguage">If <b>true</b> the method sets the default language to match the regional settings in the Control Panel.</param>
    static public void SetInitialLanguage(string originalCulture, bool setDefaultLanguage = true)
    {
      if (setDefaultLanguage)
      {
        Thread.CurrentThread.CurrentUICulture = CultureInfo.CurrentCulture;
        Thread.CurrentThread.CurrentCulture = CultureInfo.CurrentCulture;
      }

      Resources.originalCulture = originalCulture;
      Initialize();
    }

    /// <summary>
    /// Gets the assembly name.
    /// </summary>
    /// <param name="assembly">Assembly object.</param>
    /// <returns>Assembly name.</returns>
    static public string GetAssemblyName(Assembly assembly)
    {
      string result = assembly.FullName;

      int p = result.IndexOf(',');

      if (p >= 0)
        result = result.Substring(0, p);

      return result;
    }

    /// <summary>
    /// Set the assembly that contains resources.
    /// </summary>
    /// <param name="mainAssembly">Original assembly that contains the resources. If null the calling assembly is used.</param>
    static public void Initialize(Assembly mainAssembly = null)
    {
      if (mainAssembly == null)
        mainAssembly = Assembly.GetEntryAssembly();

      Resources.mainAssembly = mainAssembly;

      assemblyName = GetAssemblyName(mainAssembly);

      // Get the active culture. Get satellite assembly files matching the current culture. 
      // If not found try to get the assembly of the parent culture and so on.
      activeAssembly = null;
      culture = Thread.CurrentThread.CurrentUICulture.Name;
      CultureInfo cultureInfo = new CultureInfo(culture);

      while ((activeAssembly == null) && (cultureInfo != null) && (cultureInfo.Name != ""))
      {
        try
        {
          activeAssembly = mainAssembly.GetSatelliteAssembly(cultureInfo);
        }
        catch (Exception)
        {
        }

        cultureInfo = cultureInfo.Parent;

        if (activeAssembly == null)
          culture = cultureInfo.ToString();
      }

      // If no file found use the main assembly
      if (activeAssembly == null)
      {
        activeAssembly = mainAssembly;
        culture = "";
      }
    }

    /// <summary>
    /// Get a stream for an embedded resource.
    /// </summary>
    /// <param name="resourceName">Name of the resource</param>
    /// <returns>Stream for the resource</returns>
    static public Stream GetEmbeddedResource(string resourceName)
    {
      if (activeAssembly == null)
        Initialize(Assembly.GetCallingAssembly());

      if (activeAssembly != mainAssembly)
      {
        // Satellite assembly file in use

        // First try culture specific resource name in the satellite assembly file
        // de\Application.resources.dll: Application.Sample.de.png
        Stream value = activeAssembly.GetManifestResourceStream(GetCultureResourceName(resourceName));

        if (value != null)
          return value;

        // Try the original resource name in the satellite assembly file
        // de\MyApplication.resources.dll: Application.Sample.png
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

    static private Stream GetStandardResource(Stream resourceStream, string resourceName)
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
    static public Stream GetStandardResource(string resourceName)
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
    /// Get a stream for a file.
    /// </summary>
    /// <param name="fileName">Neutral filename</param>
    /// <returns>Stream for the localized file</returns>
    static public Stream GetFileResource(string fileName)
    {
      string thisFileName = GetCultureFileName(fileName);

      if (!File.Exists(thisFileName))
        thisFileName = fileName;

      return new FileStream(thisFileName, FileMode.Open, FileAccess.Read);
    }

    /// <summary>
    /// Check if there exists any satellite assembly files.
    /// </summary>
    static public void CheckSatelliteAssemblyCount()
    {
      if (GetAvailableLanguages().Length == 0)
        throw new Exception("In order to change language you must first create satellite assembly files for the application.");
    }

    /// <summary>
    /// Get array of available languages for the current application.
    /// </summary>
    /// <remarks>
    /// Use this method to get list of available language if you want to build your own user interface to show available languages and to select a new language.
    /// You can also use the built-in language selection dialogs. If you use Windows Forms use the <see cref="T:NewTool.Forms.SelectLanguage"/> class. 
    /// If you use WPF use the <see cref="T:NewTool.WPF.SelectLanguage"/> class.
    /// </remarks>
    /// <returns>Array of culture infos of the satellite assemblies.</returns>
    static public CultureInfo[] GetAvailableLanguages()
    {
      return GetAvailableLanguages(Assembly.GetExecutingAssembly().Location);
    }

    /// <summary>
    /// Get array of available languages for the specified application.
    /// </summary>
    /// <param name="fileName">Filename of the assembly whose satellite assemblies are listed</param>
    /// <returns>Array of culture infos of the satellite assemblies.</returns>
    static public CultureInfo[] GetAvailableLanguages(string fileName)
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