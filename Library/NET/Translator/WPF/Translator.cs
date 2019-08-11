using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Resources;
using System.Threading;
using System.Windows;
using System.Windows.Baml2006;
using System.Xaml;

namespace Soluling.WPF
{
  /// <summary>
  /// Delegate type that is used in the <see cref="E:Soluling.WPF.Translator.TranslateElementEvent"/> event.
  /// </summary>
  /// <param name="sender"></param>
  /// <param name="element"></param>
  /// <returns><b>true</b> if the event handled the element.</returns>
  public delegate bool TranslateElementEventHandler(object sender, FrameworkElement element);

  /// <summary>
  /// Delegate type that is used in the <see cref="E:Soluling.WPF.Translator.TranslateObjectEvent"/> event.
  /// </summary>
  /// <param name="sender"></param>
  /// <param name="obj"></param>
  /// <returns><b>true</b> if the event handled the object.</returns>
  public delegate bool TranslateObjectEventHandler(object sender, Object obj);

  /// <summary>
  /// Translates a specific form or all currently opened windows to the active language.
  /// </summary>
  public class Translator
  {
    private Assembly assembly = null;
    private string assemblyName = "";
    private string assemblyFileName = "";

    /// <summary>
    /// Occurs before an element is to be translated. You can disable the translation or performs your own custom translation.
    /// </summary>
    public static event TranslateElementEventHandler TranslateElementEvent;

    /// <summary>
    /// Occurs before an object is to be translated. You can disable the translation or performs your own custom translation.
    /// </summary>
    public static event TranslateObjectEventHandler TranslateObjectEvent;

    /// <summary>
    /// Initializes a new instance of the Translator class. Translates all existing windows.
    /// </summary>
    public Translator()
    {
      LoadAssembly();
      WindowCollection windows = Application.Current.Windows;

      for (int i = 0; i < windows.Count; i++)
        TranslateWindow(windows[i]);
    }

    /// <summary>
    /// Initializes a new instance of the Translator class. Translates the specified window.
    /// </summary>
    /// <param name="window">The window to be translated.</param>
    public Translator(Window window)
    {
      LoadAssembly();
      TranslateWindow(window);
    }

    private void LoadAssembly()
    {
      assembly = null;
      assemblyName = "";

      if (Language.AssemblyId != "")
      {
        string applicationFileName = Assembly.GetEntryAssembly().Location;

        assemblyFileName = Path.Combine(Path.GetDirectoryName(applicationFileName), Language.AssemblyId);
        assemblyFileName = Path.Combine(assemblyFileName, Path.GetFileNameWithoutExtension(applicationFileName)) + ".resources.dll";

        if (File.Exists(assemblyFileName))
          assembly = Assembly.LoadFrom(assemblyFileName);
        else
          assemblyFileName = "";
      }
    }

    private bool ShouldTranslateObject(Object obj)
    {
      return (TranslateObjectEvent == null) || TranslateObjectEvent(this, obj);
    }

    private void TranslateWindow(Window window)
    {
      string name = window.GetType().Name;
      BamlControl baml = LoadBaml(name);
      TranslateElement(window, baml);
    }

    private void TranslateElement(FrameworkElement element, BamlControl baml)
    {
      if ((element == null) || (baml == null))
        return;

      // Translate child controls
      IEnumerable children = LogicalTreeHelper.GetChildren(element);

      foreach (object child in children)
      {
        if (child is FrameworkElement)
        {
          FrameworkElement childElement = (FrameworkElement)child;
          TranslateElement(childElement, baml.Find(childElement));
        }
      }

      // Translate this control
      if ((TranslateElementEvent == null) || TranslateElementEvent(this, element))
      {
        PropertyInfo[] properties = element.GetType().GetProperties();

        foreach (PropertyInfo property in properties)
        {
          Object value;

          if (property.Name == "Content")
            value = baml.Value;
          else
            value = baml.FindProperty(property.Name);

          if ((value != null) && property.CanWrite && (value is string))
            //property.SetValue(element, ((string)value).ToUpper(), null);
            property.SetValue(element, value, null);
        }
      }
    }

    /// <summary>
    /// Translates all existing windows.
    /// </summary>
    static public void Translate()
    {
      new Translator();
    }

    /// <summary>
    /// Translates the specified window.
    /// </summary>
    /// <param name="window">The window to be translated.</param>
    static public void Translate(Window window)
    {
      new Translator(window);
    }

    /// <summary>
    /// Sets the new language and translates all existing windows.
    /// </summary>
    /// <param name="language">The new language of the application.</param>
    static public void SetUserInterfaceLanguage(CultureInfo language)
    {
      Language.Culture = language;
      Translate();
    }

    private BamlControl LoadBaml(string name)
    {
      if (assembly == null)
        assembly = Assembly.GetCallingAssembly();

      if (assemblyName == "")
        assemblyName = Language.GetAssemblyName(Assembly.GetCallingAssembly());

      string language = Language.Id;

      if (language == "")
        language = Thread.CurrentThread.CurrentUICulture.ToString();

      string bamlName = name.ToLower() + ".baml";

      // Get the name of g-resource 
      string resourceName = assemblyName + ".g";

      if ((language != "") && (language != Language.OriginalId))
        resourceName = resourceName + "." + language;

      resourceName = resourceName + ".resources";

      Stream stream = assembly.GetManifestResourceStream(resourceName);
      BamlControl current = null;

      using (var resourceReader = new ResourceReader(stream))
      {
        foreach (DictionaryEntry entry in resourceReader)
        {
          if (entry.Key.ToString() == bamlName)
          {
            Stream bamlStream = entry.Value as Stream; 

            var bamlReader = new Baml2006Reader(bamlStream);
            string memberName = "";

            var baml = new BamlControl();
            current = null;

            while (bamlReader.Read())
            {
              XamlNodeType nodeType = bamlReader.NodeType;

              switch (bamlReader.NodeType)
              {
                case XamlNodeType.StartObject:
                  if (current == null)
                    current = baml;
                  else
                    current = current.Add(new BamlControl());

                  if (bamlReader.Type != null)
                    current.ElementName = bamlReader.Type.ToString();

                  break;

                case XamlNodeType.GetObject:
                  current = current.Add(new BamlControl());
                  current.MemberName = memberName;
                  break;

                case XamlNodeType.EndObject:
                  if (current != null)
                    current = current.Parent;

                  break;

                case XamlNodeType.StartMember:
                  memberName = bamlReader.Member.Name;
                  break;

                case XamlNodeType.EndMember:
                  memberName = "";
                  break;

                case XamlNodeType.Value:
                  if (memberName == "Name")
                    current.Name = bamlReader.Value.ToString();
                  else if (memberName == "Uid")
                    current.Uid = bamlReader.Value.ToString();
                  else if (memberName == "Content")
                    current.Value = bamlReader.Value;
                  else if (memberName != "")
                    current.AddProperty(memberName, bamlReader.Value);

                  break;
              }
            }

            return baml;
          }
        }
      }

      return null;
    }
  }

  class BamlControl
  {
    private List<BamlControl> controls = new List<BamlControl>();
    private Dictionary<string, Object> properties = new Dictionary<string, Object>();

    public BamlControl Parent { get; set; }
    public string ElementName { get; set; }
    public string MemberName { get; set; }
    public string Name { get; set; }
    public string Uid { get; set; }
    public Object Value { get; set; }

    public BamlControl Add(BamlControl value)
    {
      controls.Add(value);
      value.Parent = this;
      return value;
    }

    public BamlControl FindMemberName(string name)
    {
      return controls.Find(control => control.MemberName == name);
    }

    public BamlControl Find(string name)
    {
      var item = controls.Find(control => control.Name == name);

      if (item == null)
        item = controls.Find(control => control.Uid == name);

      return item;
    }

    public BamlControl Find(FrameworkElement element)
    {
      string name = element.Name;

      if (name == "")
        name = element.Uid;

      BamlControl result = Find(name);

      if (result != null)
        return result;

      // Check if element is in Children
      result = FindMemberName("Children");

      if (result != null)
        result = result.Find(name);

      if (result != null)
        return result;

      // Check if element is in Items
      result = FindMemberName("Items");

      if (result != null)
        result = result.Find(name);

      return result;
    }

    public void AddProperty(string name, Object value)
    {
      properties.Add(name, value);
    }

    public Object FindProperty(string name)
    {
      if (properties.ContainsKey(name))
        return properties[name];
      else
        return null;
    }
  }
}
