using System;
using System.Windows.Forms;
using System.IO;
using System.Xml;

namespace ResxData
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void LoadNumbersXml()
    {
      // XML data is in a string resource (Properties.Resources.StringXml). 
      // Get the data as string and read it as XML. 
      // Get the contents of number elements.
      StringReader reader = new StringReader(Properties.Resources.StringXml);
      XmlTextReader xml = new XmlTextReader(reader);
      string str = "";

      while (xml.Read())
      {
        if ((xml.Name == "number") && (xml.NodeType == XmlNodeType.Element))
        {
          xml.Read();

          if (str.Length > 0)
            str = str + Environment.NewLine;

          str = str + xml.Value.ToString();
        }
      }

      stringXmlLabel.Text = str;
    }

    private void LoadSportsXml()
    {
      // XML data is in a binary resource (Properties.Resources.BinaryXml). 
      // Get the XML data and read it as XML. 
      // Get the contents of name attributes of sport elements.
      Stream stream = new MemoryStream(Properties.Resources.BinaryXml);
      XmlReader xml = XmlReader.Create(stream);
      string str = "";

      while (xml.Read())
      {
        if ((xml.Name == "sport") && (xml.NodeType == XmlNodeType.Element))
        {
          string value = xml.GetAttribute("name");

          if (str.Length > 0)
            str = str + Environment.NewLine;

          str = str + value;
        }
      }

      binaryXmlLabel.Text = str;
    }

    private void LoadSimpleXml()
    {
      // XML data is in a string resource (Properties.Resources.StringXml). 
      // Get the data as string and read it as XML. 
      // Get the contents of data element.
      StringReader reader = new StringReader(Properties.Resources.SimpleXml);
      XmlTextReader xml = new XmlTextReader(reader);

      while (xml.Read())
      {
        if ((xml.Name == "data") && (xml.NodeType == XmlNodeType.Element))
        {
          xml.Read();
          simpleXmlLabel.Text = xml.Value.ToString();
          break;
        }
      }
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      label1.Text = Properties.Resources.String1;
      pictureBox.Image = Properties.Resources.Image;
      LoadNumbersXml();
      LoadSportsXml();
      LoadSimpleXml();
    }

    private void playButton_Click(object sender, EventArgs e)
    {
      new System.Media.SoundPlayer(Properties.Resources.Sound).Play();
    }
  }
}
