using System;
using System.Drawing;
using System.Windows.Forms;

namespace NoName
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      Label label = new Label();
      label.AutoSize = false;
      label.Size = new Size(40, 10);
      label.Text = "This is a long string";
      label.Location = new Point(8, 40);
      Controls.Add(label);

      label = new Label();
      label.Name = "label3";
      label.AutoSize = false;
      label.Size = new Size(40, 10);
      label.Text = "This is another long string";
      label.Location = new Point(8, 60);
      Controls.Add(label);

      label = new Label();
      label.Name = "label3";
      label.AutoSize = false;
      label.Size = new Size(40, 10);
      label.Text = "This is yet another long string";
      label.Location = new Point(8, 80);
      Controls.Add(label);
    }

    private void Form1_Shown(object sender, EventArgs e)
    {
      NewTool.Forms.FormsChecker.CheckRoot(this);
    }
  }
}
