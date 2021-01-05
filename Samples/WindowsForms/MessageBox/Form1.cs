using System;
using System.Windows.Forms;

namespace MessageBoxApp
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void showButton_Click(object sender, EventArgs e)
    {
      MessageBox.Show(Properties.Resources.MessageBoxText, Properties.Resources.MessageBoxCaption, MessageBoxButtons.YesNoCancel);

      // There is no easy and clean way to localize the buttons of a message box. The buttons will appear in the language of the 
      // your operating system.
      // However, if you want to localize tehm you can use the trick described in the following article.
      // https://www.codeproject.com/Articles/18399/Localizing-System-MessageBox
    }
  }
}
