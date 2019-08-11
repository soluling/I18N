using System;
using System.Collections.Generic;

namespace Soluling
{
  enum DialogFilterItemType { All, Supported, Specific };

  class DialogFilterItem
  {
    internal string Pattern { get; set; }
    internal string Mask { get; set; }
    internal DialogFilterItemType Type {  get; set; }
    internal bool AddMask {  get; set; }

    public DialogFilterItem(string pattern, string mask, DialogFilterItemType type, bool addMask)
    {
      Pattern = pattern;
      Mask = mask;
      Type = type;
      AddMask = addMask;
    }
  }

  /// <summary>
  /// Defines filter string for OpenFileDialog and SaveFileDialog file dialogs.
  /// </summary>
  /// <example>
  /// <para>
  /// This example shows how to create a filter that contains all files, supported files, XML files and text files.
  /// </para>
  /// <code>
  /// var dialog = new Microsoft.Win32.OpenFileDialog();
  /// 
  /// dialog.Filter = new DialogFilter()
  ///   .AddAll("All files")
  ///   .AddSupported("Supported files")
  ///   .Add("XML files", "*.xml")
  ///   .Add("Text files", "*.txt")
  ///   .ToString();
  /// </code>
  /// <para>
  /// This will set the Filter property to "All files (*.*)|*.*|Supported files (*.xml;*.txt)|*.xml;*.txt|XML files (*.xml)|*.xml|Text files (*.txt)|*.txt".
  /// </para>
  /// <para>
  /// In a real application you would not hard code the strings into the code but use resource strings.
  /// </para>
  /// <code>
  /// var dialog = new Microsoft.Win32.OpenFileDialog();
  /// 
  /// dialog.Filter = new DialogFilter()
  ///   .AddAll(Properties.Resources.AllFiles)
  ///   .AddSupported(Properties.Resources.SupportedFiles)
  ///   .Add(Properties.Resources.XmlFiles, "*.xml")
  ///   .Add(Properties.Resources.TextFiles, "*.txt")
  ///   .ToString();
  /// </code>
  /// </example>
  public class DialogFilter
  {
    private List<DialogFilterItem> items = new List<DialogFilterItem>();

    private DialogFilter Add(string pattern, string mask, DialogFilterItemType type, bool addMask)
    {
      items.Add(new DialogFilterItem(pattern, mask, type, addMask));
      return this;
    }

    /// <summary>
    /// Adds a string for a specific file type.
    /// </summary>
    /// <param name="pattern">String that is shown when the file type is selected. Do not include masks. They are added automatically.
    /// Can contain a placeholder for mask (e.g. ({0})).</param>
    /// <param name="mask">File extension mask for the file type such as "*.xml". Can also be without * character such as "xml" or ".xml". In that case * and . characters are automatically added to the mask to complete it.</param>
    /// <param name="addMask">If true a mask will be appended into the pattern if it does not already contain a placeholder for the mask.</param>
    /// <returns>This DialogFilter class to make is easy to chain methods.</returns>
    /// <seealso cref="AddAll"/>
    /// <seealso cref="AddSupported"/>
    /// <example>
    /// Following example shows how to add the XML files item into the filter. This sample has no placeholder in the pattern parameter so the mask is appended into the pattern.
    /// The text in the filter item will be "XML files (*.xml)" instead of "XML files".
    /// <code>filter.Add("XML files", "*.xml");</code>
    /// Same as above but now the pattern contains a custom placeholder. The text in the filter item will be "XML files [*.xml]".
    /// <code>filter.Add("XML files [{0}]", "*.xml");</code>
    /// As above but no mask is added into the pattern. The text in the filter item will be "XML files".
    /// <code>filter.Add("XML files", "*.xml", false);</code>
    /// You can pass mask without * character. In that case "*." will be added to the mask. Following is same as the first sample.
    /// <code>filter.Add("XML files", "xml");</code>
    /// In a real application you would not hard code the pattern string into the code but use a resource string.
    /// <code>filter.Add(Properties.Resources.XmlFiles, "*.xml");</code>
    /// </example>
    public DialogFilter Add(string pattern, string mask, bool addMask = true)
    {
      // "" -> *.*
      // ext -> *.ext
      // .ext -> *.ext
      if (mask == "")
        mask = "*.*";
      else if (mask[0] == '.')
        mask = "*" + mask;
      else if (mask[0] != '*')
        mask = "*." + mask;

      return Add(pattern, mask, DialogFilterItemType.Specific, addMask);
    }

    /// <summary>
    /// Adds a string for all files.
    /// </summary>
    /// <param name="pattern">String that is shown when all files is selected. Do not include mask. It is added automatically.
    /// Can contain a placeholder for mask (e.g. ({0})).</param>
    /// <param name="addMask">If true a mask will be appended into the pattern if it does not already contain a placeholder for the mask.</param>
    /// <returns>This DialogFilter class to make is easy to chain methods.</returns>
    /// <seealso cref="AddSupported"/>
    /// <seealso cref="Add(string, string, bool)"/>
    /// <example>
    /// Following example shows how to add the all files item into the filter.
    /// <code>filter.AddAll("All files");</code>
    /// In a real application you would not hard code the pattern string into the code but use a resource string.
    /// <code>filter.AddAll(Properties.Resources.AllFiles);</code>
    /// </example>
    public DialogFilter AddAll(string pattern, bool addMask = true)
    {
      return Add(pattern, "*.*", DialogFilterItemType.All, addMask);
    }

    /// <summary>
    /// Adds a string for all supported files.
    /// </summary>
    /// <param name="pattern">String that is shown when all supported files is selected. Do not include masks. They are added automatically.
    /// Can contain a placeholder for mask (e.g. ({0})).</param>
    /// <param name="addMask">If true a mask will be appended into the pattern if it does not already contain a placeholder for the mask.</param>
    /// <returns>This DialogFilter class to make is easy to chain methods.</returns>
    /// <seealso cref="AddAll"/>
    /// <seealso cref="Add(string, string, bool)"/>
    /// <example>
    /// Following example shows how to add the supported files item into the filter.
    /// <code>filter.AddSupported("All supported files");</code>
    /// In a real application you would not hard code the pattern string into the code but use a resource string.
    /// <code>filter.AddSupported(Properties.Resources.AllSupportedFiles);</code>
    /// </example>
    public DialogFilter AddSupported(string pattern, bool addMask = true)
    {
      return Add(pattern, "", DialogFilterItemType.Supported, addMask);
    }

    /// <summary>
    /// Gets the filter string.
    /// </summary>
    /// <returns>Filter string.</returns>
    public override string ToString()
    {
      // Get the mask for supported file. This is a mask that contains all specific masks.
      string supportedMask = "";

      foreach (DialogFilterItem item in items)
      {
        if (item.Type == DialogFilterItemType.Specific)
        {
          if (supportedMask != "")
            supportedMask = supportedMask + ';';

          supportedMask = supportedMask + item.Mask;
        }
      }

      // Add all item to the string and return the string.
      string result = "";

      foreach (DialogFilterItem item in items)
      {
        if (item.Type == DialogFilterItemType.Supported)
          item.Mask = supportedMask;

        if (result != "")
          result = result + "|";

        result = result + GetFilter(item.Pattern, item.Mask, item.AddMask);
      }

      return result;
    }
  
    private string GetFilter(string pattern, string mask, bool checkPlaceholder)
    {
      // If the pattern does not contain a placeholder add one into end of the pattern.
      if (checkPlaceholder && (pattern.IndexOf("{0}") == -1))
        pattern = pattern + " ({0})";

      return String.Format(pattern, mask) + "|" + mask;
    }
  }
}
