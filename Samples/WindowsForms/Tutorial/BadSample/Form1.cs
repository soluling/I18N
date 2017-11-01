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
      // Hard code string!
      // Not grammatically correct when count is 1!
      // This works only if count in in the middle of the sentence!
      return "I have " + count + " skis.";
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      // Hard coded string!
      // String is too long for control and some of the string will be cut!
      label2.Text = "This is a sample";

      DateTime date = DateTime.Now;

      // Hard coded string!
      // Incorrect date to string conversion!
      label3.Text = "Todays is " + date.Month + "/" + date.Day + "/" + date.Year%100;

      label4.Text = GetSkiCount(1);
      label5.Text = GetSkiCount(2);
    }

    private void button1_Click(object sender, EventArgs e)
    {
      OpenFileDialog dialog = new OpenFileDialog();

      // Hard coded string!
      dialog.Title = "Open File";

      // Hard coded string!
      // Too complex string to translate!
      dialog.Filter = "All supported files (*.xml;*.ini)|*.xml;*.ini|XML files (*.xml)|*.xml|Ini files (*.ini)|*.ini|All files (*.*)|*.*";

      if (dialog.ShowDialog() == DialogResult.OK)
        textBox1.Text = dialog.FileName;
    }
  }
}
