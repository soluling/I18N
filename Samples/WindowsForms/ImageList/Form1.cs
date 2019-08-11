using System;
using System.Windows.Forms;

namespace ImageList
{
  public partial class Form1 : Form
  {
    private int currentImage = 0;

    public Form1()
    {
      InitializeComponent();
    }

    private void UpdateImage()
    {
      switch (currentImage)
      {
        case 0:
          label1.Text = Properties.Resources.Book;
          break;

        case 1:
          label1.Text = Properties.Resources.Sign;
          break;
      
        case 2:
          label1.Text = Properties.Resources.Plane;
          break;

        default:
          label1.Text = "";
          break;
      }

      pictureBox1.Image = imageList1.Images[currentImage];
    }

    private void button1_Click(object sender, EventArgs e)
    {
      currentImage = (currentImage + 1)%imageList1.Images.Count;
      UpdateImage();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      UpdateImage();
    }
  }
}
