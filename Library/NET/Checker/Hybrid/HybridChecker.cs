using System.Windows.Forms;
using System.Windows.Forms.Integration;
using Soluling.WPF;

namespace Soluling.Forms
{
  /// <summary>
  /// Represents the checker class that extends Windows Forms checker such way that it can check the WPF user control embedded to Windows Forms application.
  /// </summary>
  public class HybridChecker: Soluling.Forms.FormsChecker
  {
    /// <inheritdoc/>
    protected override void ProcessControl(Control control)
    {
      if (control is ElementHost)
      {
        ElementHost elementHost = (ElementHost)control;
        Control form = GetForm(elementHost);
        WpfChecker checker = new WpfChecker(this);
        checker.ScreenshotFilePrefix = form.Name + "_";

        checker.Process(elementHost.Child);
      }
    }
  }
}
