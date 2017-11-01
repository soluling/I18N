using System;
using System.Windows.Forms;

namespace Sample
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Shown(object sender, EventArgs e)
    {
      NewTool.Forms.FormsChecker.CheckRoot(this);
    }
  }
}
