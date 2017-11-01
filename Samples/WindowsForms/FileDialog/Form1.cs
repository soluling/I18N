using System;
using System.Windows.Forms;
using NewTool;

namespace FileDialog
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void openButton1_Click(object sender, EventArgs e)
    {
      OpenFileDialog dialog = new OpenFileDialog();

      dialog.Title = Properties.Resources.OpenTitle;

      // Whole filter string is exposed to translator. 
      // Even NewTool can check the validity of localized filter string this is little bit risky because translator might break the filter syntax,
      dialog.Filter = Properties.Resources.OpenFilter;

      if (dialog.ShowDialog() == DialogResult.OK)
        MessageBox.Show(dialog.FileName);
    }

    private void openButton2_Click(object sender, EventArgs e)
    {
      OpenFileDialog dialog = new OpenFileDialog();

      dialog.Title = Properties.Resources.OpenTitle;

      // This is safer then the first sample because only description text is exposed to translator
      dialog.Filter = new NewTool.DialogFilter()
        .AddSupported(Properties.Resources.AllSupportedFiles)  // All supported files is first
        .Add(Properties.Resources.XmlFiles, "xml")             // XML files
        .Add(Properties.Resources.IniFiles, "ini")             // Ini files
        .AddAll(Properties.Resources.AllFiles)                 // All files is last
        .ToString();

      if (dialog.ShowDialog() == DialogResult.OK)
        MessageBox.Show(dialog.FileName);
    }

    private void openButton3_Click(object sender, EventArgs e)
    {
      OpenFileDialog dialog = new OpenFileDialog();

      dialog.Title = Properties.Resources.OpenTitle;

      // As above sample but without All files and All supported files
      dialog.Filter = new NewTool.DialogFilter() 
        .Add(Properties.Resources.XmlFiles, "xml")  // XML files
        .Add(Properties.Resources.IniFiles, "ini")  // Ini files 
        .ToString();

      if (dialog.ShowDialog() == DialogResult.OK)
        MessageBox.Show(dialog.FileName);
    }

    private void openButton4_Click(object sender, EventArgs e)
    {
      OpenFileDialog dialog = new OpenFileDialog();

      dialog.Title = Properties.Resources.OpenTitle;

      // As original but all supported files is after XML files
      dialog.Filter = new NewTool.DialogFilter()
        .Add(Properties.Resources.XmlFiles, "xml")             // XML files
        .AddSupported(Properties.Resources.AllSupportedFiles)  // All supported files is first
        .Add(Properties.Resources.IniFiles, "ini")             // Ini files
        .AddAll(Properties.Resources.AllFiles)                 // All files is last
        .ToString();

      if (dialog.ShowDialog() == DialogResult.OK)
        MessageBox.Show(dialog.FileName);
    }

    private void saveButton1_Click(object sender, EventArgs e)
    {
      SaveFileDialog dialog = new SaveFileDialog();

      dialog.Title = Properties.Resources.SaveTitle;
      dialog.FileName = Properties.Resources.SaveFile;
      dialog.Filter = Properties.Resources.SaveFilter;

      if (dialog.ShowDialog() == DialogResult.OK)
        MessageBox.Show(dialog.FileName);
    }

    private void saveButton2_Click(object sender, EventArgs e)
    {
      SaveFileDialog dialog = new SaveFileDialog();

      dialog.Title = Properties.Resources.SaveTitle;
      dialog.FileName = Properties.Resources.SaveFile;

      dialog.Filter = new NewTool.DialogFilter() 
        .Add(Properties.Resources.XmlFiles, "xml")  // XML files
        .Add(Properties.Resources.IniFiles, "ini")  // Ini files 
        .ToString();

      if (dialog.ShowDialog() == DialogResult.OK)
        MessageBox.Show(dialog.FileName);
    }
  }
}
