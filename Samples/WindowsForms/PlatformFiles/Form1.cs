using System;
using System.Windows.Forms;

namespace PlatformFiles
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
#if CUSTOM
      label2.Text = Properties.Resources.Custom;
#else
      label2.Text = Properties.Resources.Standard;
#endif
    }
  }
}
