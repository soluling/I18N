using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace Soluling
{
  /// <summary>
  /// User interface issue type.
  /// </summary>
  public enum IssueType
  {
    /// <summary>Control has no name or id.</summary>
    NoName,

    /// <summary>Control has same name as some other control.</summary>
    DuplicateName,

    /// <summary>Part of the text is truncated.</summary>
    Truncation,

    /// <summary>Two or more controls overlap.</summary>
    Overlap,

    /// <summary>Part of the control is out of parent pounds.</summary>
    OutOfBounds
  };

  /// <summary>
  /// Represents the base class for an issue.
  /// </summary>
  public abstract class Issue
  {
    /// <summary>
    /// Get and sets the checker that created the issue.
    /// </summary>
    public UserInterfaceChecker Checker { get; set; }

    /// <summary>
    /// Gets and sets the control where the issues happens.
    /// </summary>
    public object Control { get; set; }

    /// <summary>
    /// Gets and sets the host (e.g. form, window or dialog) where the issues happens.
    /// </summary>
    public object Host { get; set; }

    /// <summary>
    /// Gets the issue type.
    /// </summary>
    public IssueType IssueType { get { return GetIssueType(); } }

    /// <summary>
    /// Gets the name of the issue type.
    /// </summary>
    public string Name { get { return GetName(); } }

    /// <summary>
    /// Gets abd sets the highlighted area in the screenshot.
    /// </summary>
    public Rectangle HightlightRect { get; set; }

    /// <summary>
    /// Get and sets the screenshot showing the issue.
    /// </summary>
    public Bitmap Screenshot { get; set; }

    /// <summary>
    /// Get and sets the screenshot file name.
    /// </summary>
    public string ScreenshotFileName { get; set; }

    /// <summary>
    /// Initializes a new instance of the Issue class.
    /// </summary>
    public Issue()
    {
    }

    /// <summary>
    /// Gets the issue type.
    /// </summary>
    /// <returns>Issue type.</returns>
    protected abstract IssueType GetIssueType();

    /// <summary>
    /// Gets the name of the issue type.
    /// </summary>
    /// <returns>Issue type as string.</returns>
    protected abstract string GetName();

    /// <summary>
    /// Writes the issue depend data into the result JSON file.
    /// </summary>
    /// <param name="writer">JSOn writer.</param>
    public virtual void WriteJson(JsonTextWriter writer)
    {
      writer.WritePropertyName("kind");
      writer.WriteValue(Name);

      writer.WritePropertyName("name");
      writer.WriteValue(Checker.GetControlName(Control));

      writer.WritePropertyName("type");
      writer.WriteValue(Control.GetType().ToString());

      writer.WritePropertyName("screenshot");
      writer.WriteValue(Path.GetFileName(ScreenshotFileName));

      writer.WritePropertyName("rectangle");
      writer.WriteValue(String.Format("{0},{1},{2},{3}", HightlightRect.X, HightlightRect.Y, HightlightRect.Width, HightlightRect.Height));
    }

    /// <summary>
    /// Removes the screenshot image from the issue.
    /// </summary>
    public void RemoveScreenshot()
    {
      Screenshot = null;
    }

    /// <summary>
    /// Saves the screenshot into the file.
    /// </summary>
    public void SaveScreenshot()
    {
      Screenshot.Save(ScreenshotFileName, System.Drawing.Imaging.ImageFormat.Png);
    }
  }

  /// <summary>
  /// Represents the name issue where control has no name or id.
  /// </summary>
  public class NoNameIssue: Issue
  {
    /// <inheritdoc/>
    protected override IssueType GetIssueType()
    {
      return IssueType.NoName;
    }

    /// <inheritdoc/>
    protected override string GetName()
    {
      return "noname";
    }
  }

  /// <summary>
  /// Represents the truncation issue that is used when the text is not completely shown on the control.
  /// </summary>
  public class TruncationIssue: Issue
  {
    /// <summary>
    /// The text size in pixels.
    /// </summary>
    public Size TextSize { get; set; }

    /// <inheritdoc/>
    protected override IssueType GetIssueType()
    {
      return IssueType.Truncation;
    }

    /// <inheritdoc/>
    protected override string GetName()
    {
      return "truncation";
    }
  }

  /// <summary>
  /// Represents the issue where other controls are also involved.
  /// </summary>
  public abstract class ControlsIssue : Issue
  {
    /// <summary>
    /// List of controls.
    /// </summary>
    protected List<object> controls = new List<object>();

    /// <summary>
    /// Number of controls that conflict with this control.
    /// </summary>
    public int Count { get { return controls.Count; } }

    /// <summary>
    /// Array of controls that conflict with this control.
    /// </summary>
    /// <param name="index">The index of the control.</param>
    /// <returns>The control that conflicts with this control.</returns>
    public object this[int index] {  get { return controls[index]; } }

    /// <summary>
    /// Adds a control that conflict with this control.
    /// </summary>
    /// <param name="control">Control to be added.</param>
    public void Add(object control)
    {
      controls.Add(control);
    }
  }

  /// <summary>
  /// Represents the name issue where control has same name as some other control.
  /// </summary>
  public class DuplicateNameIssue: ControlsIssue
  {
    /// <summary>
    /// Name of the control.
    /// </summary>
    public string ControlName { get; set; }

    /// <inheritdoc/>
    protected override IssueType GetIssueType()
    {
      return IssueType.DuplicateName;
    }

    /// <inheritdoc/>
    protected override string GetName()
    {
      return "duplicatename";
    }
  }

  /// <summary>
  /// Represents the overlap issue that is used when a control overlaps with one or more other controls.
  /// </summary>
  public class OverlapIssue: ControlsIssue
  {
    /// <inheritdoc/>
    protected override IssueType GetIssueType()
    {
      return IssueType.Overlap;
    }

    /// <inheritdoc/>
    protected override string GetName()
    {
      return "overlap";
    }

    /// <summary>
    /// Checks if a control is one of the overlap controls.
    /// </summary>
    /// <param name="control">Control to be checked.</param>
    /// <returns>True of the control is an overlap control.</returns>
    public bool Exists(object control)
    {
      return (control == Control) || controls.Exists(c => c == control);
    }

    /// <inheritdoc/>
    public override void WriteJson(JsonTextWriter writer)
    {
      base.WriteJson(writer);

      writer.WritePropertyName("overlaps");
      writer.WriteStartArray();

      foreach (object control in controls)
        writer.WriteValue(Checker.GetControlName(control));

      writer.WriteEnd();
    }
  }

  /// <summary>
  /// Represents the out of bounds issue where some part of the control is out of the parent's client area.
  /// </summary>
  public class OutOfBoundsIssue: Issue
  {
    /// <inheritdoc/>
    protected override IssueType GetIssueType()
    {
      return IssueType.OutOfBounds;
    }

    /// <inheritdoc/>
    protected override string GetName()
    {
      return "outofbounds";
    }
  }

  /// <summary>
  /// Represents a method that will be used in <see cref="E:Soluling.UserInterfaceChecker.ControlEvent"/> event.
  /// </summary>
  /// <param name="sender">The object where the event handler is attached.</param>
  /// <param name="control">The control that is currently being checked.</param>
  /// <param name="skip">Set this to true if you want to skip checking of this control.</param>
  public delegate void ControlEventHandler(UserInterfaceChecker sender, object control, ref bool skip);

  /// <summary>
  /// Represents a method that will be used in <see cref="E:Soluling.UserInterfaceChecker.IssueEvent"/> event.
  /// </summary>
  /// <param name="sender">The object where the event handler is attached.</param>
  /// <param name="issue">The issues that was found.</param>
  /// <param name="ignore">Set this to true if you want to ignore the issue.</param>
  public delegate void IssueEventHandler(UserInterfaceChecker sender, Issue issue, ref bool ignore);

  /// <summary>
  /// Represents the base checker class.
  /// </summary>
  /// <remarks>
  /// There are four ways to reduce the amount of issues that the checker writes. They are
  /// <list type="number">
  ///   <listheader>
  ///     <term>Method</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>Fix the issue</term>
  ///     <description>The best way to get rid of an issues is to fix it in your user interface. Always try this first and only if you are absolutely</description>
  ///   </item>
  ///   <item>
  ///     <term>Disable issue type</term>
  ///     <description>You can disable the issue type. This means that the checker won't check the specific issue type at all. It is easy to disable a type by using Disable method but to downside is that it will disable all issue matching that type.</description>
  ///   </item>
  ///   <item>
  ///     <term>Use event to skip controls</term>
  ///     <description>You can disable checking of controls by using <see cref="E:Soluling.UserInterfaceChecker.ControlEvent" /> event. This gives you very flexible way to control what controls to check and what not to check.</description>
  ///   </item>
  ///   <item>
  ///     <term>Use event to ignore issues found</term>
  ///     <description>You can also ignore issues found by using <see cref="E:Soluling.UserInterfaceChecker.IssueEvent" /> event. This gives you possibility to ignore an issue found.</description>
  ///   </item>
  /// </list>
  /// </remarks>
  public abstract class UserInterfaceChecker : IDisposable
  {
    private string resultsFileName;
    private Color[] highlightColors;
    private List<IssueType> disabledIssueTypes;

    /// <summary>
    /// 
    /// </summary>
    protected List<DuplicateNameIssue> duplicateNames;

    /// <summary>
    /// List of issues found.
    /// </summary>
    protected List<Issue> issues;

    /// <summary>
    /// Writer that writes the issue file.
    /// </summary>
    protected JsonTextWriter writer;

    /// <summary>
    /// Initializes a new instance of the UserInterfaceChecker class.
    /// </summary>
    public UserInterfaceChecker()
    {
      highlightColors = new Color[5];
      disabledIssueTypes = new List<IssueType>();
      issues = new List<Issue>();
      duplicateNames = new List<DuplicateNameIssue>();
      writer = null;

      HighlightIssues = true;
      HighlightMargin = 3;
      HighlightWidth = 2;

      SetHighlightColor(IssueType.NoName, Color.Aqua);
      SetHighlightColor(IssueType.DuplicateName, Color.Black);
      SetHighlightColor(IssueType.Truncation, Color.SeaGreen);
      SetHighlightColor(IssueType.Overlap, Color.Red);
      SetHighlightColor(IssueType.OutOfBounds, Color.Navy);

      try
      {
        OutputDir = Path.Combine(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "Check"), Language.Id);
        Directory.CreateDirectory(OutputDir);
      }
      catch (Exception)
      {
        OutputDir = Path.Combine(Path.GetTempPath(), "UserInterfaceChecker");
        Directory.CreateDirectory(OutputDir);
      }

      resultsFileName = Path.Combine(OutputDir, "Issues.json");

      var file = new StreamWriter(resultsFileName);

      writer = new JsonTextWriter(file);
      writer.Formatting = Formatting.Indented;
      writer.WriteStartArray();
    }

    /// <summary>
    /// Initializes a new instance of the UserInterfaceChecker class to be linked to an existing checker object.
    /// </summary>
    /// <param name="checker">Main checker where the new checker will be linked.</param>
    public UserInterfaceChecker(UserInterfaceChecker checker)
    {
      highlightColors = checker.highlightColors;
      disabledIssueTypes = checker.disabledIssueTypes;
      issues = checker.issues;
      writer = checker.writer;

      OutputDir = checker.OutputDir;
      HighlightIssues = checker.HighlightIssues;
      HighlightMargin = checker.HighlightMargin;
      HighlightWidth = checker.HighlightWidth;
    }

    /// <summary>
    /// Gets the checker extensions.
    /// </summary>
    /// <returns>Check extensions.</returns>
    protected abstract CheckerExtensions GetExtensions();

    /// <summary>
    /// Gets and sets the screenshot file name prefix. Default is empty.
    /// </summary>
    public string ScreenshotFilePrefix { get; set; }

    /// <summary>
    /// Gets and sets the output directory where screenshots and the issue files are written.
    /// </summary>
    public string OutputDir { get; set; }

    /// <summary>
    /// Gets and sets the flags that specifies if issues are highlighted on the screenshots.
    /// </summary>
    public bool HighlightIssues { get; set; }

    /// <summary>
    /// Gets and sets the margin in pixels between the control and the highlight rectangle.
    /// </summary>
    public int HighlightMargin { get; set; }

    /// <summary>
    /// Gets and sets the width of the hightlight rectangle.
    /// </summary>
    public int HighlightWidth { get; set; }

    /// <summary>
    /// Occurs when a control is about to be checked.
    /// </summary>
    /// <remarks>
    /// This event can be used to skip checking of certain controls.
    /// </remarks>
    public event ControlEventHandler ControlEvent;

    /// <summary>
    /// Occurs when an issues is found.
    /// </summary>
    /// <remarks>
    /// This event can be used to ignore certain issues.
    /// </remarks>
    public event IssueEventHandler IssueEvent;

    /// <summary>
    /// Implement this to check the root control and its child controls.
    /// </summary>
    /// <param name="root">Root control to be checked.</param>
    public abstract void Process(object root);

    /// <summary>
    /// Gets the control name.
    /// </summary>
    /// <param name="control">Control.</param>
    /// <returns>Name of th control.</returns>
    public abstract string GetControlName(object control);

    /// <summary>
    /// Takes a screenshot about the issue.
    /// </summary>
    /// <param name="issue">Issue</param>
    public abstract void DoTakeScreenshot(Issue issue);

    /// <summary>
    /// Takes a screenshot about the issue.
    /// </summary>
    /// <param name="issue">Issue</param>
    protected void TakeScreenshot(Issue issue)
    {
      GetExtensions().Show(this, issue.Control);
      DoTakeScreenshot(issue);
      GetExtensions().Restore(issue.Control);
    }

    /// <summary>
    /// Checks if a issue type is disabled.
    /// </summary>
    /// <param name="value">Issue type.</param>
    /// <returns>true if given issue type is disabled.</returns>
    public bool IsDisabled(IssueType value)
    {
      return disabledIssueTypes.Contains(value);
    }

    /// <summary>
    /// Disables an issue type.
    /// </summary>
    /// <param name="value">Issue type.</param>
    public void Disable(IssueType value)
    {
      disabledIssueTypes.Add(value);
    }

    /// <summary>
    /// Enables an issue type.
    /// </summary>
    /// <param name="value">Issue type.</param>
    public void Enable(IssueType value)
    {
      if (disabledIssueTypes.Contains(value))
        disabledIssueTypes.Remove(value);
    }

    /// <summary>
    /// Checks a control and its child controls.
    /// </summary>
    /// <param name="root">Control to be checked.</param>
    /// <returns>This UserInterfaceChecker class to make is easy to chain methods.</returns>
    public UserInterfaceChecker Check(object root)
    {
      DoCheck(root);
      return this;
    }

    /// <summary>
    /// Waits the specific time and then checks a control and its child controls.
    /// </summary>
    /// <param name="root">Control to be checked.</param>
    /// <param name="delay">Time in milliseconds that the checking is delayed.</param>
    public async Task CheckAsync(object root, int delay)
    {
      await Task.Delay(delay);
      DoCheck(root);
    }

    /// <summary>
    /// Waits the specific time and then checks a control and its child controls.
    /// </summary>
    /// <param name="root">Control to be checked.</param>
    /// <param name="delay">Time in milliseconds that the checking is delayed.</param>
    public async void CheckWithDelay(object root, int delay)
    {
      await Task.Delay(delay);
      DoCheck(root);
    }

    /// <summary>
    /// Gets the duplicate name issues matching the control name.
    /// </summary>
    /// <param name="name">The control name.</param>
    /// <returns>The issue if any. <b>null</b> is none.</returns>
    protected DuplicateNameIssue GetDuplicateNameIssue(string name)
    {
      return duplicateNames.Find(issue => issue.ControlName == name);
    }

    /// <summary>
    /// Adds a control names into the control name list..
    /// </summary>
    /// <param name="host">The form or window.</param>
    /// <param name="control">The control.</param>
    /// <returns>The issue.</returns>
    protected DuplicateNameIssue AddDuplicateNameIssue(object host, object control)
    {
      var result = new DuplicateNameIssue();
      result.Checker = this;
      result.ControlName = GetControlName(control);
      result.Control = control;
      result.Host = host;
      duplicateNames.Add(result);

      return result;
    }

    private void DoCheck(object root)
    {
      writer.WriteStartObject();

      writer.WritePropertyName("name");
      writer.WriteValue(GetControlName(root));

      writer.WritePropertyName("type");
      writer.WriteValue((root.GetType().ToString()));

      writer.WritePropertyName("issues");
      writer.WriteStartArray();

      duplicateNames.Clear();

      Process(root);

      // Add the duplicate name issues having more than one countrol
      foreach (var issue in duplicateNames)
      {
        if (issue.Count > 0)
        {
          DoTakeScreenshot(issue);

          if (ShouldWriteIssue(issue))
            AddIssue(issue);
        }
      }

      writer.WriteEnd();

      writer.WriteEndObject();
    }

    /// <summary>
    /// Gets the highlight color.
    /// </summary>
    /// <param name="issueType">The issue type.</param>
    /// <returns>Highlight color.</returns>
    public Color GetHighlightColor(IssueType issueType)
    {
      return highlightColors[(int)issueType];
    }

    /// <summary>
    /// Sets the highlight color.
    /// </summary>
    /// <param name="issueType">The issue type.</param>
    /// <param name="value">Highlight color.</param>
    /// <returns>This UserInterfaceChecker class to make is easy to chain methods.</returns>
    public UserInterfaceChecker SetHighlightColor(IssueType issueType, Color value)
    {
      highlightColors[(int)issueType] = value;
      return this;
    }

    /// <summary>
    /// Closes the checker.
    /// </summary>
    public void Dispose()
    {
      End();
    }

    /// <summary>
    /// Ends the checker.
    /// </summary>
    public void End()
    {
      if (writer != null)
      {
        writer.WriteEnd();
        writer.Close();
        writer = null;
      }
    }

    /// <summary>
    /// Determines whether the truncation checking is disabled of a specific control.
    /// </summary>
    /// <param name="control">Control to be checked.</param>
    /// <returns>true if the truncation check is disabled.</returns>
    protected bool IsTruncationDiabled(object control)
    {
      return IsDisabled(IssueType.Truncation) || GetExtensions().Ignore(control, new List<IssueType> { IssueType.Truncation });
    }

    /// <summary>
    /// Determines whether the issue should be written.
    /// </summary>
    /// <param name="issue">The issues to be written.</param>
    /// <returns><b>true</b> if the issues should be written.</returns>
    protected bool ShouldWriteIssue(Issue issue)
    {
      if (IssueEvent != null)
      {
        var ignore = false;

        IssueEvent(this, issue, ref ignore);
        return !ignore;
      }
      else
        return true;
    }

    /// <summary>
    /// Determines whether checking of the control should be ignored.
    /// </summary>
    /// <param name="control">The control to be checked.</param>
    /// <returns><b>true</b> if the control should be checked.</returns>
    protected bool ShouldSkipControl(object control)
    {
      if (ControlEvent != null)
      {
        var skip = false;

        ControlEvent(this, control, ref skip);
        return skip;
      }
      else
        return false;
    }

    /// <summary>
    /// Determines whether the overlap issue is unique.
    /// </summary>
    /// <param name="issue">The overlap issue.</param>
    /// <returns><b>true</b> if the issue is unique.</returns>
    protected bool IsOverlapIssueUnique(OverlapIssue issue)
    {
      foreach (var testIssue in issues)
      {
        if (testIssue is OverlapIssue)
        {
          var thisIssue = (OverlapIssue)testIssue;

          if (thisIssue.Count == issue.Count)
          {
            var match = thisIssue.Exists(issue.Control);

            if (match)
            {
              for (int i = 0; i < issue.Count; i++)
              {
                if (!thisIssue.Exists(issue[i]))
                {
                  match = false;
                  break;
                }
              }
            }

            if (match)
              return false;
          }
        }
      }

      return true;
    }

    /// <summary>
    /// Adds the issue in the issue list.
    /// </summary>
    /// <param name="issue">The issues to be added.</param>
    protected void AddIssue(Issue issue)
    {
      // Write issue to the issue file
      writer.WriteStartObject();

      issue.WriteJson(writer);

      writer.WriteEnd();

      // Save the screenshot file
      issue.SaveScreenshot();
      issue.RemoveScreenshot();

      // Add the issues to the issue list
      issues.Add(issue);
    }
  }

  /// <summary>
  /// Represents the base class for checker extensions.
  /// </summary>
  public class CheckerExtension
  {
    /// <summary>
    /// Make sure that the control is visible.
    /// </summary>
    /// <param name="checker">Checker object.</param>
    /// <param name="control">The control to be checked.</param>
    /// <param name="restoreControl">The state to be restored. <b>null</b> if none.</param>
    /// <returns><b>true</b> if the control was set visible.</returns>
    public virtual bool Show(UserInterfaceChecker checker, object control, out object restoreControl)
    {
      restoreControl = null;
      return false;
    }

    /// <summary>
    /// Restores a control that was previously set visible.
    /// </summary>
    /// <param name="control">The control to be checked.</param>
    /// <param name="restoreControl">The state to be restored.</param>
    /// <returns><b>true</b> if the control was restored.</returns>
    public virtual bool Restore(object control, object restoreControl)
    {
      return false;
    }

    /// <summary>
    /// Determines if the control should be ignored.
    /// </summary>
    /// <param name="control">The control to be checked.</param>
    /// <param name="ignoredIssueTypes">The issue type.</param>
    /// <returns><b>true</b> if the control should be ignored.</returns>
    public virtual bool Ignore(object control, List<IssueType> ignoredIssueTypes)
    {
      return false;
    }
  }

  /// <summary>
  /// Specifies a checker extension list.
  /// </summary>
  public class CheckerExtensions
  {
    private object RestoreControl;
    private List<CheckerExtension> items = new List<CheckerExtension>();

    /// <summary>
    /// Gets the count of extensions.
    /// </summary>
    public int Count { get { return items.Count; } }

    /// <summary>
    /// Gets an extension.
    /// </summary>
    /// <param name="index">The index of the extension.</param>
    /// <returns>The extension.</returns>
    public CheckerExtension this[int index] {  get { return items[index]; } }

    /// <summary>
    /// Make sure that the control is visible.
    /// </summary>
    /// <param name="checker">Checker object.</param>
    /// <param name="control">The control to be checked.</param>
    /// <returns><b>true</b> if the control was set visible.</returns>
    public bool Show(UserInterfaceChecker checker, object control)
    {
      object restoreControl;

      if (Show(checker, control, out restoreControl))
      {
        RestoreControl = restoreControl;
        return true;
      }
      else
        return false;
    }

    /// <summary>
    /// Make sure that the control is visible.
    /// </summary>
    /// <param name="checker">Checker object.</param>
    /// <param name="control">The control to be checked.</param>
    /// <param name="restoreControl">The state to be restored. <b>null</b> if none.</param>
    /// <returns><b>true</b> if the control was set visible.</returns>
    public bool Show(UserInterfaceChecker checker, object control, out object restoreControl)
    {
      for (int i = 0; i < Count; i++)
        if (this[i].Show(checker, control, out restoreControl))
          return true;

      restoreControl = null;
      return false;
    }

    /// <summary>
    /// Restores a control that was previously set visible.
    /// </summary>
    /// <param name="control">The control to be checked.</param>
    /// <returns><b>true</b> if the control was restored.</returns>
    public bool Restore(object control)
    {
      return Restore(control, RestoreControl);
    }

    /// <summary>
    /// Restores a control that was previously set visible.
    /// </summary>
    /// <param name="control">The control to be checked.</param>
    /// <param name="restoreControl">The state to be restored.</param>
    /// <returns><b>true</b> if the control was restored.</returns>
    public bool Restore(object control, object restoreControl)
    {
      for (int i = 0; i < Count; i++)
        if (this[i].Restore(control, restoreControl))
          return true;

      return false;
    }

    /// <summary>
    /// Determines if the control should be ignored.
    /// </summary>
    /// <param name="control">The control to be checked.</param>
    /// <param name="ignoredIssueTypes">The issue type.</param>
    /// <returns><b>true</b> if the control should be ignored.</returns>
    public bool Ignore(object control, List<IssueType> ignoredIssueTypes)
    {
      for (int i = 0; i < Count; i++)
        if (this[i].Ignore(control, ignoredIssueTypes))
          return true;

      return false;
    }

    /// <summary>
    /// Registers an extension.
    /// </summary>
    /// <param name="value">The extension to be registered.</param>
    public void Register(CheckerExtension value)
    {
      items.Add(value);
    }
  }
}
