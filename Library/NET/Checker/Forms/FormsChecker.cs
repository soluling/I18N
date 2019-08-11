using System;
using System.Collections.Generic;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace Soluling.Forms
{
  /// <summary>
  /// Represents the Windows Forms checker class.
  /// </summary>
  public class FormsChecker: UserInterfaceChecker
  {
    static private CheckerExtensions extensions = new CheckerExtensions();

    static FormsChecker()
    {
      Register(new StandardCheckerExtension());
    }

    /// <summary>
    /// Registers an extension.
    /// </summary>
    /// <param name="value">The extension to be registered.</param>
    public static void Register(CheckerExtension value)
    {
      extensions.Register(value);
    }

    /// <summary>
    /// Initializes a new instance of the FormsChecker class.
    /// </summary>
    public FormsChecker(): base()
    {
    }

    /// <summary>
    /// Initializes a new instance of the FormsChecker class to be linked to an existing checker object.
    /// </summary>
    /// <param name="checker">Main checker where the new checker will be linked.</param>
    public FormsChecker(UserInterfaceChecker checker): base(checker)
    {
    }

    /// <inheritdoc/>
    protected override CheckerExtensions GetExtensions()
    {
      return extensions;
    }

    /// <summary>
    /// Checks a control.
    /// </summary>
    /// <param name="control">The control to be checked.</param>
    public static void CheckRoot(Control control)
    {
      new FormsChecker().Check(control).End();
    }

    /// <summary>
    /// Checks a control.
    /// </summary>
    /// <param name="control">The control to be checked.</param>
    /// <param name="delay">Time in milliseconds that the checking is delayed.</param>
    public static async void CheckRootAsync(Control control, int delay)
    {
      FormsChecker checker = new FormsChecker();
      await checker.CheckAsync(control, delay);
      checker.End();
    }

    /// <inheritdoc/>
    public override void Process(object root)
    {
      Process((Control)root);
    }

    /// <inheritdoc/>
    public override string GetControlName(object control)
    {
      return ((Control)control).Name;
    }

    /// <inheritdoc/>
    public override void DoTakeScreenshot(Issue issue)
    {
      // Get control and the form
      var control = (Control)issue.Control;
      var form = (Form)issue.Host;

      issue.ScreenshotFileName = OutputDir + "\\" + form.Name + "_" + control.Name + "_" + issue.Name + ".png";

      // Get screenshot bitmap
      var screenshot = new Bitmap(form.Width, form.Height);

      // Take the screenshot
      form.DrawToBitmap(screenshot, new Rectangle(0, 0, screenshot.Width, screenshot.Height));

      // Draw highlight if needed
      if (HighlightIssues)
      {
        var highlightRect = issue.HightlightRect;

        highlightRect.Inflate(HighlightMargin, HighlightMargin);
        highlightRect.Location = new Point(highlightRect.Left + 8, highlightRect.Top + SystemInformation.CaptionHeight + 8);
        highlightRect = control.Parent.RectangleToScreen(highlightRect);
        highlightRect = form.RectangleToClient(highlightRect);

        var graphics = Graphics.FromImage(screenshot);
        graphics.DrawRectangle(new Pen(GetHighlightColor(issue.IssueType), HighlightWidth), highlightRect);
        graphics.Flush();
      }

      // Assign the screenshot to the issue
      issue.Screenshot = screenshot;
    }

    private Issue CreateIssue(Type type, Control control, Rectangle hightlight)
    {
      var form = GetForm(control);
      var issue = (Issue)Activator.CreateInstance(type);

      issue.Checker = this;
      issue.Control = control;
      issue.Host = form;
      issue.HightlightRect = hightlight;

      TakeScreenshot(issue);

      return issue;
    }

    /// <summary>
    /// Abstract method that can be used to perform a special checking for certain controls.
    /// </summary>
    /// <param name="control">The control to be checked.</param>
    protected virtual void ProcessControl(Control control)
    {
    }

    private void Process(Control control)
    {
      if (extensions.Ignore(control, new List<IssueType>()) || ShouldSkipControl(control))
        return;

      if (control is Label)
        CheckLabel((Label)control);
      else if (control is Button)
        CheckButton((Button)control);
      else if (control is CheckBox)
        CheckCheckBox((CheckBox)control);
      else if (control is RadioButton)
        CheckRadioButton((RadioButton)control);
      else if (control is CheckedListBox)
        CheckCheckedListBox((CheckedListBox)control);
      else if (control is ListBox)
        CheckListBox((ListBox)control);
      else if (control is ComboBox)
        CheckComboBox((ComboBox)control);
      else
        ProcessControl(control);

      CheckNoName(control);
      CheckDuplicateName(control);
      CheckOverlap(control);
      CheckOutOfBounds(control);

      foreach (var child in control.Controls)
        Process(child);
    }

    /// <summary>
    /// Gets the form where the control belongs to.
    /// </summary>
    /// <param name="control">The control.</param>
    /// <returns>The form.</returns>
    protected Control GetForm(Control control)
    {
      var result = control.Parent;

      while (!(result is Form))
        result = result.Parent;

      return result;
    }

    private int CalculateTextWidth(Control control, string text)
    {
      return TextRenderer.MeasureText(text, control.Font, control.Size, TextFormatFlags.NoPrefix).Width;
    }

    private Size CalculateTextSize(Control control, string text, int leftMargin)
    {
      var size = control.Size;

      size.Width -= leftMargin;
      size = TextRenderer.MeasureText(text, control.Font, size, TextFormatFlags.NoPrefix | TextFormatFlags.WordBreak);
      size.Width += leftMargin;

      return size;
    }

    private void CheckLabel(Label label)
    {
      if (label.AutoSize || IsTruncationDiabled(label))
        return;

      var size = CalculateTextSize(label, label.Text, 0);

      if ((size.Width > label.Width) || (size.Height > label.Height))
        ProcessTruncation(label, size);
    }

    private void CheckCheckBox(CheckBox checkBox)
    {
      if (checkBox.AutoSize || IsTruncationDiabled(checkBox))
        return;

      var size = CalculateTextSize(checkBox, checkBox.Text, GetSystemMetrics(SM_CXMENUCHECK) + GetSystemMetrics(SM_CXEDGE));

      if ((size.Width > checkBox.Width) || (size.Height > checkBox.Height))
        ProcessTruncation(checkBox, size);
    }

    private void CheckRadioButton(RadioButton radioButton)
    {
      if (radioButton.AutoSize || IsTruncationDiabled(radioButton))
        return;

      var size = CalculateTextSize(radioButton, radioButton.Text, GetSystemMetrics(SM_CXMENUCHECK));

      if ((size.Width > radioButton.Width) || (size.Height > radioButton.Height))
        ProcessTruncation(radioButton, size);
    }

    private void CheckButton(Button button)
    {
      if (button.AutoSize || IsTruncationDiabled(button))
        return;

      var size = CalculateTextSize(button, button.Text, 4*GetSystemMetrics(SM_CXEDGE));

      if ((size.Width > button.Width) || (size.Height > button.Height))
        ProcessTruncation(button, size);
    }

    private void CheckComboBox(ComboBox comboBox)
    {
      if (IsTruncationDiabled(comboBox))
        return;

      foreach (var value in comboBox.Items)
      {
        Size size = CalculateTextSize(comboBox, comboBox.GetItemText(value), GetSystemMetrics(SM_CXVSCROLL) + 2*GetSystemMetrics(SM_CXBORDER));

        if (size.Width > comboBox.Width)
        {
          ProcessTruncation(comboBox, size);
          break;
        }
      }
    }

    private void CheckListBox(ListBox listBox)
    {
      if (IsTruncationDiabled(listBox))
        return;

      foreach (var value in listBox.Items)
      {
        Size size = CalculateTextSize(listBox, listBox.GetItemText(value), 2*GetSystemMetrics(SM_CXBORDER));

        if (size.Width > listBox.Width)
        {
          ProcessTruncation(listBox, size);
          break;
        }
      }
    }

    private void CheckCheckedListBox(CheckedListBox checkedListBox)
    {
      if (IsTruncationDiabled(checkedListBox))
        return;

      foreach (var value in checkedListBox.Items)
      {
        var size = CalculateTextSize(checkedListBox, checkedListBox.GetItemText(value), + GetSystemMetrics(SM_CXMENUCHECK) + 2*GetSystemMetrics(SM_CXBORDER));

        if (size.Width > checkedListBox.Width)
        {
          ProcessTruncation(checkedListBox, size);
          break;
        }
      }
    }

    private void ProcessTruncation(Control control, Size size)
    {
      var issue = (TruncationIssue)CreateIssue(typeof(TruncationIssue), control, control.Bounds);

      issue.TextSize = size;
      
      if (ShouldWriteIssue(issue))
        AddIssue(issue);
    }

    private void CheckNoName(Control control)
    {
      if ((control.Parent == null) || IsDisabled(IssueType.NoName) || extensions.Ignore(control,  new List<IssueType> { IssueType.NoName }))
        return;

      if (control.Name == "")
      {
        var issue = CreateIssue(typeof(NoNameIssue), control, control.Bounds);

        if (ShouldWriteIssue(issue))
          AddIssue(issue);
      }
    }

    private void CheckDuplicateName(Control control)
    {
      if ((control.Parent == null) || (control.Name == "") || IsDisabled(IssueType.DuplicateName) || extensions.Ignore(control,  new List<IssueType> { IssueType.DuplicateName }))
        return;

      var issue = GetDuplicateNameIssue(GetControlName(control));

      if (issue != null)
        issue.Add(control);
      else
      {
        issue = AddDuplicateNameIssue(GetForm(control), control);
        issue.HightlightRect = control.Bounds;
      }
    }

    private void CheckOverlap(Control control)
    {
      if ((control.Parent == null) || IsDisabled(IssueType.Overlap) || extensions.Ignore(control, new List<IssueType> { IssueType.Overlap }))
        return;

     var overlaps = new List<Control>();
      var highlightRect = control.Bounds;

      foreach (Control thisControl in control.Parent.Controls)
        if ((thisControl != control) && (control.Bounds.IntersectsWith(thisControl.Bounds)))
        {
          overlaps.Add(thisControl);
          highlightRect = Rectangle.Union(highlightRect, thisControl.Bounds);
        }

      if (overlaps.Count == 0)
        return;

      var issue = (OverlapIssue)CreateIssue(typeof(OverlapIssue), control, highlightRect);

      foreach (var overlap in overlaps)
        issue.Add(overlap);

      if (IsOverlapIssueUnique(issue) && ShouldWriteIssue(issue))
        AddIssue(issue);
    }

    private void CheckOutOfBounds(Control control)
    {
      if ((control.Parent == null) || IsDisabled(IssueType.OutOfBounds) || extensions.Ignore(control,  new List<IssueType> { IssueType.OutOfBounds }))
        return;

      if (((control.Location.X + control.Width) > control.Parent.ClientSize.Width) || 
        ((control.Location.Y + control.Height) > control.Parent.ClientSize.Height))
      {
        var issue = CreateIssue(typeof(OutOfBoundsIssue), control, control.Bounds);

        if (ShouldWriteIssue(issue))
          AddIssue(issue);
      }
    }

    private const int SM_CXVSCROLL = 2;
    private const int SM_CXBORDER = 5;
    private const int SM_CXEDGE = 45;
    private const int SM_CXMENUCHECK = 71;

    [DllImport("user32.dll")]
    static extern int GetSystemMetrics(int smIndex);
  }

  /// <summary>
  /// Represents the base class for Windows Forms checker extensions.
  /// </summary>
  public class FormsCheckerExtension : CheckerExtension
  {
    /// <summary>
    /// Find a typed ancestor. It is either a direct parent or any parent above the direct parent that matches the given type.
    /// </summary>
    /// <param name="control">The control.</param>
    /// <param name="type">The type of the ancestor</param>
    /// <returns>The ancestor control.</returns>
    protected Control FindAncestor(Control control, Type type)
    {
      var result = control.Parent;

      while (result != null)
      {
        if (result.GetType() == type)
          break;
        else
          result = result.Parent;
      }

      return result;
    }
  }

  class StandardCheckerExtension : FormsCheckerExtension
  {
    public override bool Show(UserInterfaceChecker checker, object control, out object restoreControl)
    {
      restoreControl = null;
      var tabPage = (TabPage)FindAncestor((Control)control, typeof(TabPage));

      if (tabPage != null)
      {
        var tabControl = (TabControl)tabPage.Parent;
        restoreControl = tabControl.SelectedTab;
        tabControl.SelectedTab = tabPage;

        return true;
      }
      else
        return false;
    }

    public override bool Restore(object control, object restoreControl)
    {
      if (restoreControl is TabPage)
      {
        var tabPage = (TabPage)restoreControl;
        var tabControl = (TabControl)tabPage.Parent;
        tabControl.SelectedTab = tabPage;
        return true;
      }
      else
        return false;
    }

    public override bool Ignore(object control, List<IssueType> issueTypes)
    {
      return (control is TabPage) && issueTypes.Contains(IssueType.Overlap);
    }
  }
}
