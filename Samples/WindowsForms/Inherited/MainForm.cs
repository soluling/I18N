using System;
using System.Windows.Forms;

namespace WindowsForms
{
  public partial class MainForm : Form
  {
    public MainForm()
    {
      InitializeComponent();
    }

    private void simpleButton_Click(object sender, EventArgs e)
    {
      new SimpleForm().Show();
    }

    private void baseButton_Click(object sender, EventArgs e)
    {
      new BaseForm().Show();
    }

    private void inheritedButton_Click(object sender, EventArgs e)
    {
      new InheritedForm().Show();
    }

    private void moreButton_Click(object sender, EventArgs e)
    {
      new MoreForm().Show();
    }
  }
}
