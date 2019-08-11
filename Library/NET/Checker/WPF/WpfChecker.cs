using System;
using System.Globalization;
using System.Windows;
using System.Windows.Media;
using System.Windows.Controls;
using System.Collections.Generic;

namespace Soluling.WPF
{
  /// <summary>
  /// Represents the WPF checker class.
  /// </summary>
  public class WpfChecker: UserInterfaceChecker
  {
    static private CheckerExtensions extensions = new CheckerExtensions();

    static WpfChecker()
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
    /// Initializes a new instance of the WpfChecker class.
    /// </summary>
    public WpfChecker(): base()
    {
    }

    /// <summary>
    /// Initializes a new instance of the WpfChecker class to be linked to an existing checker object.
    /// </summary>
    /// <param name="checker">Main checker where the new checker will be linked.</param>
    public WpfChecker(UserInterfaceChecker checker): base(checker)
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
    /// <param name="element">The framework element to be checked.</param>
    public static void CheckRoot(FrameworkElement element)
    {
      new WpfChecker().Check(element).End();
    }

    /// <summary>
    /// Checks a control.
    /// </summary>
    /// <param name="element">The framework element to be checked.</param>
    /// <param name="delay">Time in milliseconds that the checking is delayed.</param>
    public static async void CheckRoot(FrameworkElement element, int delay)
    {
      WpfChecker checker = new WpfChecker();
      await checker.CheckAsync(element, delay);
      checker.End();
    }

    /// <inheritdoc/>
    public override void Process(object root)
    {
      ProcessElement((FrameworkElement)root);
    }

    /// <inheritdoc/>
    public override string GetControlName(object control)
    {
      return GetName((FrameworkElement)control);
    }

    /// <inheritdoc/>
    public override void DoTakeScreenshot(Issue issue)
    {
      // Get element and the window
      var element = (FrameworkElement)issue.Control;
      var host = (FrameworkElement)issue.Host;

      System.Drawing.Rectangle windowRect;
      Point point;

      if (host is Window)
      {
        var window = (Window)host;
        point = new Point(window.Left, window.Top);
        windowRect = new System.Drawing.Rectangle((int)window.Left, (int)window.Top, (int)window.Width, (int)window.Height);
      }
      else
      {
        point = host.PointToScreen(new Point(0, 0));
        windowRect = new System.Drawing.Rectangle((int)point.X, (int)point.Y, (int)host.ActualWidth, (int)host.ActualHeight);
      }

      // Get screenshot bitmap
      System.Drawing.Bitmap screenshot = new System.Drawing.Bitmap(windowRect.Width, windowRect.Height);

      using (System.Drawing.Graphics graphics = System.Drawing.Graphics.FromImage(screenshot))
      {
        // Take the screenshot
        graphics.CopyFromScreen(windowRect.Left, windowRect.Top, 0, 0, screenshot.Size);

        // Draw highlight if needed
        if (HighlightIssues)
        {
          var location = host.PointToScreen(new Point(0, 0));
          var dx = (int)(location.X - point.X);
          var dy = (int)(location.Y - point.Y);
          System.Drawing.Rectangle highlightRect = new System.Drawing.Rectangle(issue.HightlightRect.Left + dx, issue.HightlightRect.Top + dy, issue.HightlightRect.Width, issue.HightlightRect.Height);
          highlightRect.Inflate(HighlightMargin, HighlightMargin);

          graphics.DrawRectangle(new System.Drawing.Pen(GetHighlightColor(issue.IssueType), HighlightWidth), highlightRect);
          graphics.Flush();
        }
      }

      // Assign the screenshot to the issue
      issue.Screenshot = screenshot;
    }

    private void ProcessElement(FrameworkElement element)
    {
      if (element.Visibility == Visibility.Hidden)
        return;

      if (element is Label)
        CheckLabel((Label)element);
      else if (element is CheckBox)
        CheckCheckBox((CheckBox)element);
      else if (element is RadioButton)
        CheckRadioButton((RadioButton)element);
      else if (element is Button)
        CheckButton((Button)element);
      else if (element is ListBox)
        CheckListBox((ListBox)element);
      else if (element is ComboBox)
        CheckComboBox((ComboBox)element);

      CheckOverlap(element);
      CheckOutOfBounds(element);

      var children = LogicalTreeHelper.GetChildren(element);

      object restoreControl;
      extensions.Show(this, element, out restoreControl);

      foreach (var child in children)
        if (child is FrameworkElement)
          ProcessElement((FrameworkElement)child);

      extensions.Restore(element, restoreControl);
    }

    private string GetName(FrameworkElement element)
    {
      var result = element.Name;

      if (result == "")
        result = element.Uid;

      if (result == "")
        result = element.GetType().Name;

      return result;
    }

    private double CalculateTextWidth(Control control, string text)
    {
      return CalculateTextSize(control, text).Width;
    }

    private Size CalculateTextSize(Control control, string text)
    {
       var dpiScale = VisualTreeHelper.GetDpi(control);

       var ft = new FormattedText(
         text,
         CultureInfo.CurrentCulture,
         FlowDirection.LeftToRight,
         new Typeface(control.FontFamily, control.FontStyle, control.FontWeight, control.FontStretch),
         control.FontSize,
         Brushes.Black,
         dpiScale.PixelsPerDip);

      return new Size(ft.Width, ft.Height);
    }

    private Issue CreateIssue(Type type, FrameworkElement element, Rect hightlight)
    {
      var issue = (Issue)Activator.CreateInstance(type);
      issue.Checker = this;
      issue.Control = element;

      var window = Window.GetWindow(element);

      if (window != null)
      {
        issue.Host = window;
      }
      else
      {
        var host = element;

        while ((host != null) && !(host is UserControl) && !(host is Window) && !(host is Page))
          host = (FrameworkElement)VisualTreeHelper.GetParent(host);

        issue.Host = host;
      }
       
      issue.HightlightRect = new System.Drawing.Rectangle((int)hightlight.Left, (int)hightlight.Top, (int)hightlight.Width, (int)hightlight.Height);

      // Get the screenshot name
      var name = "";
      var thisElement = element;

      while (thisElement != null)
      {
        if ((name == "") || (thisElement is UserControl) || (thisElement is Window) || (thisElement is Page))
        {
          if (name != "")
            name = "_" + name;

          name = GetName(thisElement) + name;
        }

        thisElement = (FrameworkElement)VisualTreeHelper.GetParent(thisElement);

        if ((thisElement != null) && (thisElement.GetType().ToString() == "System.Windows.Forms.Integration.AvalonAdapter"))
          break;
      }

      issue.ScreenshotFileName = OutputDir + "\\" + ScreenshotFilePrefix + name + "_" + issue.Name + ".png";

      // Take the screenshot
      TakeScreenshot(issue);

      return issue;
    }

    private Rect GetRect(FrameworkElement element)
    {
      var window = Window.GetWindow(element);
      var location = element.TranslatePoint(new Point(0, 0), window);
      var rectangleBounds = new Rect();
      rectangleBounds = element.RenderTransform.TransformBounds(new Rect(location.X, location.Y, element.ActualWidth, element.ActualHeight));
      return rectangleBounds;
    }

    private System.Drawing.Size SizeToDrawingSize(Size value)
    {
      return new System.Drawing.Size((int)value.Width, (int)value.Height);
    }

    private void ProcessTruncation(FrameworkElement element, Size size)
    {
      var issue = (TruncationIssue)CreateIssue(typeof(TruncationIssue), element, GetRect(element));
      issue.TextSize = SizeToDrawingSize(size);
      
      if (ShouldWriteIssue(issue))
        AddIssue(issue);
    }

    private void CheckLabel(Label label)
    {
      if ((label.Width == double.NaN) || IsTruncationDiabled(label) || !(label.Content is string))
        return;

      var size = CalculateTextSize(label, (string)label.Content);

      if (size.Width > label.Width)
        ProcessTruncation(label, size);
    }

    private void CheckCheckBox(CheckBox checkBox)
    {
      if ((checkBox.Width == double.NaN) || IsTruncationDiabled(checkBox) || !(checkBox.Content is string))
        return;

      var size = CalculateTextSize(checkBox, (string)checkBox.Content);

      if (size.Width > checkBox.Width)
        ProcessTruncation(checkBox, size);
    }

    private void CheckRadioButton(RadioButton radio)
    {
      if ((radio.Width == double.NaN) || IsTruncationDiabled(radio) || !(radio.Content is string))
        return;

      var size = CalculateTextSize(radio, (string)radio.Content);

      if (size.Width > radio.Width)
        ProcessTruncation(radio, size);
    }

    private void CheckButton(Button button)
    {
      if ((button.Width == double.NaN) || IsTruncationDiabled(button) || !(button.Content is string))
        return;

      var size = CalculateTextSize(button, (string)button.Content);

      if (size.Width > button.Width)
        ProcessTruncation(button, size);
    }

    private void CheckListBox(ListBox listBox)
    {
      if (IsTruncationDiabled(listBox))
        return;

      foreach (var value in listBox.Items)
      {
        var size = CalculateTextSize(listBox, value.ToString()); // + 2*GetSystemMetrics(SM_CXBORDER);

        if (size.Width > listBox.Width)
        {
          ProcessTruncation(listBox, size);
          break;
        }
      }
    }

    private void CheckComboBox(ComboBox comboBox)
    {
      if (IsTruncationDiabled(comboBox))
        return;

      foreach (var value in comboBox.Items)
      {
        var size = CalculateTextSize(comboBox, value.ToString()); // + 2*GetSystemMetrics(SM_CXBORDER);

        if (size.Width > comboBox.Width)
        {
          ProcessTruncation(comboBox, size);
          break;
        }
      }
    }

    private void CheckOverlap(FrameworkElement element)
    {
      if ((element.Parent == null) || IsDisabled(IssueType.Overlap) || extensions.Ignore(element, new List<IssueType> { IssueType.Overlap }))
        return;

      var rect = GetRect(element);
      var highlightRect = rect;
      var overlaps = new List<FrameworkElement>();
      var parent = (FrameworkElement)VisualTreeHelper.GetParent(element);
      var children = LogicalTreeHelper.GetChildren(parent);

      foreach (var child in children)
      {
        if (child is FrameworkElement)
        {
          var childElement = (FrameworkElement)child;
          var childRect = GetRect(childElement);

          if ((childElement != element) && childRect.IntersectsWith(rect))
          {
            overlaps.Add(childElement);
            highlightRect = Rect.Union(highlightRect, childRect);
          }
        }
      }

      if (overlaps.Count == 0)
        return;

      var issue = (OverlapIssue)CreateIssue(typeof(OverlapIssue), element, highlightRect);

      foreach (var overlap in overlaps)
        issue.Add(overlap);

      if (IsOverlapIssueUnique(issue) && ShouldWriteIssue(issue))
        AddIssue(issue);
    }

    private void CheckOutOfBounds(FrameworkElement element)
    {
      if ((element.Parent == null) || IsDisabled(IssueType.OutOfBounds) || extensions.Ignore(element,  new List<IssueType> { IssueType.OutOfBounds }))
        return;

      FrameworkElement parent = (FrameworkElement)VisualTreeHelper.GetParent(element);

      if (parent == null)
        return;

      var location = element.TranslatePoint(new Point(0, 0), parent);

      if (((location.X + element.ActualWidth) > parent.ActualWidth) || ((location.Y + element.Height) > parent.ActualHeight))
      {
        var issue = CreateIssue(typeof(OutOfBoundsIssue), element, GetRect(element));

        if (ShouldWriteIssue(issue))
          AddIssue(issue);
      }
    }
  }

  /// <summary>
  /// Represents the base class for WPF checker extensions.
  /// </summary>
  public class WpfCheckerExtension : CheckerExtension
  {
    private delegate void NoArgDelegate();

    /// <summary>
    /// Refreshes the element.
    /// </summary>
    /// <param name="obj">The element to be refreshed.</param>
    protected static void Refresh(DependencyObject obj)
    {
      obj.Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.ApplicationIdle, (NoArgDelegate)delegate() { });
    }
  }

  class StandardCheckerExtension : WpfCheckerExtension
  {
    public override bool Show(UserInterfaceChecker checker, object control, out object restoreControl)
    {
      restoreControl = null;

      if (control is TabItem)
      {
        var tabItem = (TabItem)control;
        var tabControl = (TabControl)tabItem.Parent;
        restoreControl = tabControl.SelectedItem;
        tabControl.SelectedItem = tabItem;
        Refresh(tabControl);
        return true;
      }
      else if (control is Expander)
      {
        var expander = (Expander)control;

        if (!expander.IsExpanded)
          restoreControl = expander;

        expander.IsExpanded = true;
        Refresh(expander);
        return true;
      }
      else
        return false;
    }

    public override bool Restore(object control, object restoreControl)
    {
      if (restoreControl is TabItem)
      {
        var tabItem = (TabItem)restoreControl;
        var tabControl = (TabControl)tabItem.Parent;
        tabControl.SelectedItem = tabItem;
        Refresh(tabControl);
        return true;
      }
      else if (restoreControl is Expander)
      {
        var expander = (Expander)restoreControl;
        expander.IsExpanded = false;
        Refresh(expander);
        return true;
      }
      else
        return false;
    }

    public override bool Ignore(object control, List<IssueType> issueTypes)
    {
      return ((control is TabItem) || (control is Expander)) && issueTypes.Contains(IssueType.Overlap);
    }
  }
}
