using System;
using System.Windows.Forms;

namespace WindowsForms
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      label2.Text = Properties.Resources.String1;
    }
  }
}
