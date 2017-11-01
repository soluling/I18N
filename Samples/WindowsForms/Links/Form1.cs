using System;
using System.Text;
using System.Windows.Forms;

namespace Links
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      pictureBox1.Image = Properties.Resources.ImageEmbedded;
      pictureBox2.Image = Properties.Resources.ImageLinked;

      label1.Text = Properties.Resources.String;

      textXml.Text = Properties.Resources.XmlText;
      binaryXml.Text = Encoding.UTF8.GetString(Properties.Resources.XmlBinary);
    }
  }
}
