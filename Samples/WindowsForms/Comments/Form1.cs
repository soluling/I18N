using System;
using System.Windows.Forms;

namespace Comments
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      var name = "Bill";
      var place = "Helsinki";

      label1.Text = Properties.Resources.HelloWorld;

      label2.Text = String.Format(Properties.Resources.FormatNoComment, name, place);
      label2.Text = String.Format(Properties.Resources.FormatComment, name, place);
      label2.Text = String.Format(Properties.Resources.FormatPlaceholderComment, name, place);
    }
  }
}
