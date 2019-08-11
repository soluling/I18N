using System;
using System.Windows.Forms;

namespace BiDiArabic
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void browseButton_Click(object sender, EventArgs e)
    {
      textBox.Text = "";
    }
  }
}
