using System;
using System.Windows.Forms;
using ResourceStringLocation.Properties;

namespace ResourceStringLocation
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      label1.Text = String.Format(Properties.Resources.MyName, "Jack");

      label2.Text = Resources.String2;
      label3.Text = Resources.String3;
    }

    private void Form1_DoubleClick(object sender, EventArgs e)
    {
      new Form2().Show();
    }
  }
}
