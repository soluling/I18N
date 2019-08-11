using System;
using System.Windows.Forms;

namespace SinglePlural
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private string Process(int count)
    {
      return Soluling.MultiPattern.Format(Properties.Resources.MessagePlural, count, count);
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      Soluling.Language.Id = Properties.Resources.Language;

      label0.Text = Process(0);
      label1.Text = Process(1);
      label2.Text = Process(2);
      label3.Text = Process(3);
      label4.Text = Process(4);
      label5.Text = Process(5);
      label11.Text = Process(11);
      label21.Text = Process(21);
      label101.Text = Process(101);
      label111.Text = Process(111);
    }
  }
}
