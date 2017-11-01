using System.Windows.Forms;

namespace WinFormsSample
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Shown(object sender, System.EventArgs e)
    {
      NewTool.Forms.HybridChecker.CheckRoot(this);
    }
  }
}
