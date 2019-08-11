using System;
using System.Windows.Forms;

namespace Lists
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Add(string name, string display)
    {
      Data data = new Data(name, display);

      listBox2.Items.Add(data);
      checkedListBox2.Items.Add(data);
      comboBox2.Items.Add(data);
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      listBox2.DisplayMember = "Display";
      checkedListBox2.DisplayMember = "Display";
      comboBox2.DisplayMember = "Display";

      Add("0", "This is a very"); // long string");
      Add("1", "One");
      Add("2", "Two");
      Add("3", "Three");
    }

    private void Form1_Shown(object sender, EventArgs e)
    {
      NewTool.Forms.FormsChecker.CheckRoot(this);
    }
  }

  class Data
  {
    public string Name { get; set; }
    public string Display { get; set; }

    public Data(string name, string display)
    {
      Name = name;
      Display = display;
    }
  }
}
