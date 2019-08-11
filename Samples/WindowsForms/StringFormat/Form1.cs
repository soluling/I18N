using System;
using System.Windows.Forms;

namespace StringFormat
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      label1.Text = String.Format(Properties.Resources.Hello, "John");
      label2.Text = String.Format(Properties.Resources.ServerError, "MyServer", "Could not connect");
      label3.Text = String.Format(Properties.Resources.CalculationNone, 1, 2, 3);
      label4.Text = String.Format(Properties.Resources.CalculationPartial, 2, 3, 5);
      label5.Text = String.Format(Properties.Resources.Test, "{}");
    }
  }
}
