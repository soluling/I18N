using System;
using System.Drawing;
using System.Windows.Forms;
using NewTool;
// To use NewTool add ..\..\Library\NET\Resources.cs as a link to the project

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
      // Initialize NewTool's resource engine to use this assembly and any satellite assembly file it has loaded
      Resources.Initialize(this.GetType().Assembly);

      runtimeResxPictureBox.Image = Properties.Resources.delete;
      resourcePictureBox.Image = new Bitmap(Resources.GetStandardResource("airplane.png"));
      embeddedResourcePictureBox.Image = new Bitmap(Resources.GetEmbeddedResource("car_sedan_blue.png"));
    }
  }
}
