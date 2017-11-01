using System;
using System.Windows.Forms;
using System.Reflection;
using System.Resources;

namespace SatelliteContract
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      label2.Text = Properties.Resources.String1;

      Assembly assembly = Assembly.GetExecutingAssembly();
      Version version = assembly.GetName().Version;

      object[] attributes = assembly.GetCustomAttributes(typeof(SatelliteContractVersionAttribute), false);

      if (attributes.Length > 0)
      {
        SatelliteContractVersionAttribute versionAttribute = attributes[0] as SatelliteContractVersionAttribute;
      }
    }
  }
}
