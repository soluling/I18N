using System;
using System.Windows.Forms;

namespace MultiPlural
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    public bool UseZero { get { return useZeroCheck.Checked; } }

    private string Process(uint completed, uint total)
    {
      if (UseZero)
        return Soluling.MultiPattern.FormatMulti(Properties.Resources.ZeroMessagePlural, completed, total);
      else
        return Soluling.MultiPattern.FormatMulti(Properties.Resources.MessagePlural, completed, total);
    }

    private void UpdateValues()
    {
      label0_0.Text = Process(0, 0);
      label0_1.Text = Process(0, 1);
      label1_1.Text = Process(1, 1);
      label0_2.Text = Process(0, 2);
      label1_2.Text = Process(1, 2);
      label2_2.Text = Process(2, 2);
      label1_3.Text = Process(1, 3);
      label2_3.Text = Process(2, 3);
      label1_10.Text = Process(1, 10);
      label5_10.Text = Process(5, 10);
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      Soluling.Language.Id = Properties.Resources.Language;
      UpdateValues();
    }

    private void useZeroCheck_CheckedChanged(object sender, EventArgs e)
    {
      UpdateValues();
    }
  }
}
