using System;
using System.Reflection;
using System.Resources;
using System.Windows.Forms;

namespace WindowsFormsApp
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      label1.Text = Properties.Resources.String1;

      // Shared project does not generate .Designed.cs file so we need to use the resource manager to access the strings from the shared projects
      ResourceManager rm = new ResourceManager("WindowsFormsApp.Values", Assembly.GetExecutingAssembly());
      label2.Text = rm.GetString("String1");

      ResourceManager rm2 = new ResourceManager("WindowsFormsApp.resources.StringRes", Assembly.GetExecutingAssembly());
      label3.Text = rm2.GetString("String1");
    }
  }
}
