using System;
using System.ComponentModel;
using System.Globalization;
using System.Windows.Forms;

namespace NewTool.Forms
{
  /// <summary>
  /// Delegate type that is used in the <see cref="E:NewTool.Forms.Translator.TranslateControlEvent"/> event.
  /// </summary>
  /// <param name="sender">Sender of the event.</param>
  /// <param name="control">Control that is about the be translated.</param>
  /// <returns><b>true</b> if the event handled the control.</returns>
  public delegate bool TranslateControlEventHandler(object sender, Control control);

  /// <summary>
  /// Delegate type that is used in the <see cref="E:NewTool.Forms.Translator.TranslateObjectEvent"/> event.
  /// </summary>
  /// <param name="sender">Sender of the event.</param>
  /// <param name="obj">Object that is about the be translated.</param>
  /// <returns><b>true</b> if the event handled the object.</returns>
  public delegate bool TranslateObjectEventHandler(object sender, Object obj);

  /// <summary>
  /// Translates a specific form or all currently opened windows to the active language.
  /// </summary>
  public class Translator
  {
    private ComponentResourceManager resourceManager = null;
    private ToolTip toolTip = null;

    /// <summary>
    /// Occurs before a control is to be translated. You can disable the translation or performs your own custom translation.
    /// </summary>
    public static event TranslateControlEventHandler TranslateControlEvent;

    /// <summary>
    /// Occurs before an object is to be translated. You can disable the translation or performs your own custom translation.
    /// </summary>
    public static event TranslateObjectEventHandler TranslateObjectEvent;

    private bool ShouldTranslateObject(Object obj)
    {
      return (TranslateObjectEvent == null) || TranslateObjectEvent(this, obj);
    }

    /// <summary>
    /// Initializes a new instance of the Translator class. Translates all existing forms.
    /// </summary>
    public Translator()
    {
      FormCollection forms = Application.OpenForms;

      for (int i = 0; i < forms.Count; i++)
        TranslateForm(forms[i]);
    }

    /// <summary>
    /// Initializes a new instance of the Translator class. Translates the specified form.
    /// </summary>
    /// <param name="form">The form to be translated.</param>
    public Translator(Form form)
    {
      TranslateForm(form);
    }

    private void TranslateGridViewColumns(DataGridViewColumnCollection items)
    {
      for (int i = 0; i < items.Count; i++)
      {
        DataGridViewColumn item = items[i];

        if (!ShouldTranslateObject(item))
          continue;

        resourceManager.ApplyResources(item, item.Name);
      }
    }

    private void TranslateToolStripItems(ToolStripItemCollection items)
    {
      for (int i = 0; i < items.Count; i++)
      {
        ToolStripItem item = items[i];

        if (!ShouldTranslateObject(item))
          continue;

        resourceManager.ApplyResources(item, item.Name);

        if (item is ToolStripDropDownItem)
          TranslateToolStripItems((item as ToolStripDropDownItem).DropDownItems);
      }
    }

    private void TranslateListViewColumns(ListView.ColumnHeaderCollection items)
    {
      for (int i = 0; i < items.Count; i++)
      {
        ColumnHeader item = items[i];

        if (!ShouldTranslateObject(item))
          continue;

        string name = item.Name;

        if ((name == "") && (item.Tag is string))
          name = (string)item.Tag;

        resourceManager.ApplyResources(item, name);
      }
    }

    private void TranslateListViewItems(ListView.ListViewItemCollection items)
    {
      for (int i = 0; i < items.Count; i++)
      {
        ListViewItem item = items[i];

        if (!ShouldTranslateObject(item))
          continue;

        string name = item.ListView.Name + ".Items";

        if (i > 0)
          name = name + i.ToString();

        ListViewItem source = resourceManager.GetObject(name) as ListViewItem;
        item.Text = source.Text;

        for (int j = 0; j < item.SubItems.Count; j++)
          item.SubItems[j].Text = source.SubItems[j].Text;
      }
    }

    private void TranslateControl(Control control, string name)
    {
      // Translate child controls
      if (control.Controls.Count > 0)
      {
        for (int i = 0; i < control.Controls.Count; i++)
        {
          Control childControl = control.Controls[i];
          TranslateControl(childControl, childControl.Name);
        }
      }

      // Translate this control
      if ((TranslateControlEvent == null) || TranslateControlEvent(this, control))
      {
        resourceManager.ApplyResources(control, name);
        toolTip.SetToolTip(control, resourceManager.GetString(name + ".ToolTip"));

        if (control is MenuStrip)
          TranslateToolStripItems(((MenuStrip)control).Items);
        else if (control is DataGridView)
          TranslateGridViewColumns(((DataGridView)control).Columns);
        else if (control is ListView)
        {
          ListView listView = (ListView)control;
          TranslateListViewColumns(listView.Columns);
          TranslateListViewItems(listView.Items);
        }
      }
    }

    private void LayoutControl(Control control)
    {
      control.ResumeLayout(false);
      control.PerformLayout();
    }

    private void TranslateForm(Form form)
    {
      int left = form.Left;
      int top = form.Top;
      int width = form.Width;
      int height = form.Height;

      resourceManager = new ComponentResourceManager(form.GetType());
      toolTip = new ToolTip();

      TranslateControl(form, "$this");

      form.Left = left;
      form.Top = top;
      form.Width = width;
      form.Height = height;

      for (int i = 0; i < form.Controls.Count; i++)
        LayoutControl(form.Controls[i]);
    
      LayoutControl(form);
    }

    /// <summary>
    /// Translates all existing forms.
    /// </summary>
    static public void Translate()
    {
      new Translator();
    }

    /// <summary>
    /// Translates the specified form.
    /// </summary>
    /// <param name="form">Form to be translated.</param>
    static public void Translate(Form form)
    {
      new Translator(form);
    }

    /// <summary>
    /// Sets the new language and translates all existing forms.
    /// </summary>
    /// <param name="language">The new language of the application.</param>
    static public void SetUserInterfaceLanguage(CultureInfo language)
    {
      Resources.SetCulture(language);
      Translate();
    }

    /// <summary>
    /// Mirrors the specified control.
    /// </summary>
    /// <param name="control">Control to be mirrored.</param>
    /// <param name="includeChildren">If <b>true</b> also all child controls will be mirrored.</param>
    static public void FlipControl(Control control, bool includeChildren)
    {
      for (int i = 0; i < control.Controls.Count; i++)
      {
        Control thisControl = control.Controls[i];
        thisControl.Left = control.Width - (thisControl.Left + thisControl.Width);

        if (includeChildren)
          FlipControl(thisControl, includeChildren);
      }
    }
  }
}