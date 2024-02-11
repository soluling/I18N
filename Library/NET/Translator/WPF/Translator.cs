using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Resources;
using System.Threading;
using System.Windows;
using System.Windows.Baml2006;
using System.Windows.Documents;
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
  public delegate bool TranslateObjectEventHandler(object sender, object obj);

  /// <summary>
  /// Translates a specific form or all currently opened windows to the active language.
  /// </summary>
  public class Translator
  {
    public static string RUN = "System.Windows.Documents.Run";

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

    private bool ShouldTranslateObject(object obj)
    {
      return (TranslateObjectEvent == null) || TranslateObjectEvent(this, obj);
    }

    private void TranslateWindow(Window window)
    {
      string name = window.GetType().Name;
      BamlControl baml = LoadBaml(name);
      TranslateElement(window, baml);
    }

    private void TranslateRun(Run run, BamlControl baml)
    {
      if (baml == null)
        return;

      object value = baml.FindProperty("Text");

      if ((value != null) && (value is string))
        run.Text = (string)value;
    }

    private void TranslateElement(FrameworkElement element, BamlControl baml)
    {
      if ((element == null) || (baml == null))
        return;

      // Translate child controls
      IEnumerable childrenEnum = LogicalTreeHelper.GetChildren(element);
      object[] children = childrenEnum.Cast<object>().ToArray();

      for (var i  = 0; i < children.Length; i++)
      {
        object child = children[i];

        if (child is FrameworkElement)
        {
          FrameworkElement childElement = (FrameworkElement)child;
          TranslateElement(childElement, baml.Find(childElement));
        }
        else if (child is Run)
        {
          Run run = (Run)child;
          TranslateRun(run, baml.FindInline(run, i));
        }
      }

      // Translate this control
      if ((TranslateElementEvent == null) || TranslateElementEvent(this, element))
      {
        PropertyInfo[] properties = element.GetType().GetProperties();

        foreach (PropertyInfo property in properties)
        {
          object value;

          if (property.Name == "Content")
            value = baml.Value;
          else
            value = baml.FindProperty(property.Name);

          if ((value != null) && (value is string) && property.CanWrite)
          {
            try
            {
              property.SetValue(element, value, null);
            }
            catch
            { 
              // Reason for this might be an invalid translation. Do not one error fail the whole translation process.
            }
          }
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

    private BamlControl GetPrevious(BamlControl control)
    {
      var parent = control.Parent;
      var index = parent.controls.IndexOf(control);

      if (index > 0)
        return parent.controls[index - 1];

      return null;
    }

    private BamlControl LoadBaml(string name)
    {
      if (assembly == null)
        assembly = Assembly.GetEntryAssembly();

      if (assemblyName == "")
        assemblyName = Language.GetAssemblyName(Assembly.GetEntryAssembly());

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

      if ((stream == null) && (language != "") && (language == Language.OriginalId))
      {
        resourceName = assemblyName + ".g." + language + ".resources";
        stream = assembly.GetManifestResourceStream(resourceName);
      }

      if (stream == null)
        return null;

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
                  {
                    current = baml;

                    if (bamlReader.Type != null)
                      current.ElementName = bamlReader.Type.ToString();
                  }
                  else
                  {
                    var newCurrent = new BamlControl();

                    if (bamlReader.Type != null)
                      newCurrent.ElementName = bamlReader.Type.ToString();

                    if (newCurrent.ElementName == RUN)
                    {
                      var itemsProperty = current.DeleteProperty("_Items");

                      if( (itemsProperty != null) && !string.IsNullOrWhiteSpace(itemsProperty.ToString()))
                      {
                        var run = new BamlControl();
                        run.ElementName = newCurrent.ElementName;
                        run.AddProperty("Text", itemsProperty.ToString());

                        current.Add(run);
                      }
                    }

                    current = current.Add(newCurrent);
                  }

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
                  {
                    current.AddProperty(memberName, bamlReader.Value);

                    if (current.ElementName == RUN)
                    {
                      var previous = GetPrevious(current);

                      if ((previous != null) && 
                        (previous.ElementName == RUN) && 
                        string.IsNullOrWhiteSpace(bamlReader.Value.ToString()) &&
                        string.IsNullOrWhiteSpace(previous.Text))
                      {
                        current.Parent.controls.Remove(previous);
                      }
                    }
                  }
                  else if (current.IsRun)
                  {
                    // If this and previous are all white space, ignore this
                    if (string.IsNullOrWhiteSpace(bamlReader.Value.ToString()) && string.IsNullOrWhiteSpace(current.controls.Last().Text))
                      break;

                    var run = new BamlControl();
                    run.ElementName = RUN;
                    run.AddProperty("Text", bamlReader.Value.ToString());
                    current.Add(run);
                  }

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
    internal List<BamlControl> controls = new List<BamlControl>();
    private Dictionary<string, object> properties = new Dictionary<string, object>();

    public BamlControl Parent { get; set; }
    public string ElementName { get; set; }
    public string MemberName { get; set; }
    public string Name { get; set; }
    public string Uid { get; set; }
    public object Value { get; set; }

    public bool IsRun 
    { 
      get
      { 
        var result = false;

        foreach (var control in controls) 
        { 
          if (control.ElementName != Translator.RUN)
            return false;
          else
            result = true;
        }

        return result;
      }
    }

    public string Text
    {
      get
      {
        var property = FindProperty("Text");

        if (property != null)
          return property.ToString();
        else
          return "";
      }
    }

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

    public BamlControl FindInline(Inline element, int index)
    {
      foreach (var control in controls)
      {
        foreach (var subControl in control.controls)
        {
          if (subControl.Name == element.Name)
            return subControl;
        }
      }

      foreach (var control in controls)
      {
        foreach (var subControl in control.controls)
        {
          if (subControl.Uid == element.Name)
            return subControl;
        }
      }

      if ((element.Name == "") && controls.Count > 0)
      {
        if (controls[0].MemberName == "Inlines")
        {
          var parent = controls[0];

          if (index < parent.controls.Count)
            return parent.controls[index];
        }
      }

      return null;
    }

    public BamlControl Find(string name, FrameworkElement element)
    {
      // Find by Name or x:Name attribute
      var item = controls.Find(control => control.Name == name);

      // Find bu Uid or x:Uid attribute
      if (item == null)
        item = controls.Find(control => control.Uid == name);

      // Find by type name
      if (item == null)
      {
        var typeName = element.GetType().FullName;

        item = controls.Find(control => control.ElementName == typeName);
      }

      return item;
    }

    public BamlControl Find(FrameworkElement element)
    {
      string name = element.Name;

      if (name == "")
        name = element.Uid;

      BamlControl result = Find(name, element);

      if (result != null)
        return result;

      // Check if element is in Children
      result = FindMemberName("Children");

      if (result != null)
        result = result.Find(name, element);

      if (result != null)
        return result;

      // Check if element is in Items
      result = FindMemberName("Items");

      if (result != null)
        result = result.Find(name, element);

      return result;
    }

    public void AddProperty(string name, object value)
    {
      properties.Add(name, value);
    }

    public object DeleteProperty(string name)
    {
      var result = FindProperty(name);

      if (result != null)
      {
        properties.Remove(name);
      }

      return result;
    }

    public object FindProperty(string name)
    {
      if (properties.ContainsKey(name))
        return properties[name];
      else
        return null;
    }
  }
}
