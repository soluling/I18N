using System;
using System.Windows.Forms;
using Soluling.Forms;

namespace Sport
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void UpdateItems()
    {
      // You have to set the tooltips on runtime because Visual Studio does not serialize
      // them into the form resource.
      listView1.Items[0].ToolTipText = Properties.Resources.Soccer;
      listView1.Items[1].ToolTipText = Properties.Resources.Hockey;
      listView1.Items[2].ToolTipText = Properties.Resources.Basketball;
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      UpdateItems();
    }

    private void button1_Click(object sender, EventArgs e)
    {
      // Show a language select dialog and turn on the selected language
      if (SelectLanguage.Select())
      {
        // Language has been changed.
        // Properties that were set on run time must be reset.
        UpdateItems();
      }
    }
  }
}
