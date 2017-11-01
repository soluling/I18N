using System;
using System.Windows.Forms;
using NewTool;

namespace CustomPlural
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Process(int count, Label label1, Label label2, Label label3, Label label4, Label label5)
    {
      label1.Text = MultiPattern.Format(Properties.Resources.MessagePlural, count, count);
      label2.Text = MultiPattern.Format(Properties.Resources.ZeroMessagePlural, count, count);
      label3.Text = MultiPattern.Format(Properties.Resources.TwoMessagePlural, count, count);
      label4.Text = MultiPattern.Format(Properties.Resources.Two2MessagePlural, count, count);
      label5.Text = MultiPattern.Format(Properties.Resources.TextMessagePlural, count, count);
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      Process(0, label0_0, label0_1, label0_2, label0_3, label0_4);
      Process(1, label1_0, label1_1, label1_2, label1_3, label1_4);
      Process(2, label2_0, label2_1, label2_2, label2_3, label2_4);
      Process(3, label3_0, label3_1, label3_2, label3_3, label3_4);
      Process(11, label4_0, label4_1, label4_2, label4_3, label4_4);
      Process(20, label5_0, label5_1, label5_2, label5_3, label5_4);
    }
  }
}
