using System;
using System.Drawing;
using System.Windows.Forms;
using Soluling;

namespace Image
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      // This image has been added into .resx
      runtimeResxPictureBox.Image = Properties.Resources.delete;

      // This image has been added as a Resource into the project
      resourcePictureBox.Image = new Bitmap(Language.GetStandardResource("airplane.png"));

      // This image has been added as an Embedded Resource into the project
      embeddedResourcePictureBox.Image = new Bitmap(Language.GetEmbeddedResource("car_sedan_blue.png"));
    }
  }
}
