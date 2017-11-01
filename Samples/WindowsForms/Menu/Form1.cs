using System;
using System.Windows.Forms;

namespace Menu
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void exitToolStripMenuItem_Click(object sender, EventArgs e)
    {
      Close();
    }

    private void openToolStripMenuItem_Click(object sender, EventArgs e)
    {
      openToolStripMenuItem.ShortcutKeys = Keys.Control | Keys.B;
    }
  }
}
