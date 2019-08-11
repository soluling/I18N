using System;
using System.Windows.Forms;
using ResourceStringLocation.Properties;

namespace ResourceStringLocation
{
  public partial class Form2 : Form
  {
    public Form2()
    {
      InitializeComponent();
    }

    private void Form2_Load(object sender, EventArgs e)
    {
      label1.Text = String.Format(Properties.Resources.MyName, "Jill");

      label2.Text = Resources.String2;
    }
  }
}
