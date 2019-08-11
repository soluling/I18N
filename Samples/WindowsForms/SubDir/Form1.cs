using System.Windows.Forms;
using SubDir.Sub;

namespace SubDir
{
  public partial class MainForm : Form
  {
    public MainForm()
    {
      InitializeComponent();
    }

    private void MainForm_MouseDoubleClick(object sender, MouseEventArgs e)
    {
      new SubForm().ShowDialog();
    }
  }
}
