using System;
using System.Windows.Forms;

namespace TreeView
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      TreeNode node = treeView1.Nodes[2];

      node.Nodes.Add("1", Properties.Resources.String1);
      node.Nodes.Add("2", Properties.Resources.String2);
      node.Nodes.Add("3", Properties.Resources.String3);
    }
  }
}
