/*
 *  A sample that shows how to use abbreviated numbers.
 */
using System;
using System.Windows.Forms;
using NewTool;
using NewTool.Forms;

namespace Number
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    public AbbreviatedNumberForm Form
    {
      get
      {
        if (longRadio.Checked)
          return AbbreviatedNumberForm.Long;
        else if (shortRadio.Checked)
          return AbbreviatedNumberForm.Short;
        else
          return AbbreviatedNumberForm.Currency;
      }
    }

    public int Precision
    {
      get
      {
        return (int)precisionUpDown.Value;
      }
    }

    private void Process(double value, Label abbreviated, Label original)
    {
      string template;

      if (Form == AbbreviatedNumberForm.Currency)
        template =  Properties.Resources.CurrencySample;
      else
        template = Properties.Resources.NumberSample;

      abbreviated.Text = String.Format(template, AbbreviatedNumber.Format(Form, value, Precision));
      original.Text = String.Format(template, Convert.ToUInt64(value).ToString());
    }

    private void UpdateValues()
    {
      Process(1, abbreviated1, original1);
      Process(2.4, abbreviated2, original2);
      Process(121, abbreviated3, original3);
      Process(1000, abbreviated4, original4);
      Process(1650, abbreviated5, original5);
      Process(27450, abbreviated6, original6);
      Process(190394, abbreviated7, original7);
      Process(3400000, abbreviated8, original8);
      Process(54000000, abbreviated9, original9);
      Process(670000000, abbreviated10, original10);
      Process(1200000000, abbreviated11, original11);
      Process(20200000000, abbreviated12, original12);
      Process(410000000000, abbreviated13, original13);
      Process(9520000000000, abbreviated14, original14);
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      UpdateValues();
    }

    private void Radio_Click(object sender, EventArgs e)
    {
      UpdateValues();
    }

    private void precisionUpDown_ValueChanged(object sender, EventArgs e)
    {
      UpdateValues();
    }

    private void button1_Click(object sender, EventArgs e)
    {
      if (SelectLanguage.Select())
        UpdateValues();
    }
  }
}
