using System;
using System.Windows.Forms;
using NewTool;
using NewTool.Forms;

namespace Events
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void OnControl(UserInterfaceChecker sender, object control, ref bool skip)
    {
      // Skip label1 control
      skip = control == label1;
    }

    public void OnIssue(UserInterfaceChecker sender, Issue issue, ref bool ignore)
    {
      // Ignore all overlap issues
      ignore = issue is OverlapIssue;
    }

    private void Form1_Shown(object sender, EventArgs e)
    {
      FormsChecker checker = new FormsChecker();

      // There are four ways you can reduce amount of issues
      // 1) Fix the issue

      // 2) Do not check out of bounds issues
      checker.Disable(IssueType.OutOfBounds);

      // 3) Add control event that can be used to skip controls
      checker.ControlEvent += OnControl;

      // 4) Add issues event that can be used to ignore certain issues
      checker.IssueEvent += OnIssue;

      checker.Check(this);
      checker.End();
    }
  }
}
