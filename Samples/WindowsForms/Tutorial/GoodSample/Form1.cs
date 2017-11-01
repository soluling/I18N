using System;
using System.Windows.Forms;

namespace Tutorial
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private string GetSkiCount(int count)
    {
      // Use Plural.Format instead of String.Format whenever the placeholder contains a number
      return NewTool.MultiPattern.Format(Properties.Resources.SkisPlural, count);
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      label2.Text = Properties.Resources.SampleText;
      label3.Text = String.Format(Properties.Resources.TodayIs, DateTime.Now.ToShortDateString());
      label4.Text = GetSkiCount(1);
      label5.Text = GetSkiCount(2);
    }

    private void button1_Click(object sender, EventArgs e)
    {
      OpenFileDialog dialog = new OpenFileDialog();

      dialog.Title = Properties.Resources.OpenTitle;

      // This is safer than putting the whole filter into single resource string because only description texts are exposed to translator
      dialog.Filter = new NewTool.DialogFilter()
        .AddSupported(Properties.Resources.AllSupportedFiles)
        .Add(Properties.Resources.XmlFiles, "xml")
        .Add(Properties.Resources.IniFiles, "ini")
        .AddAll(Properties.Resources.AllFiles)
        .ToString();

      if (dialog.ShowDialog() == DialogResult.OK)
        textBox1.Text = dialog.FileName;
    }
  }
}
