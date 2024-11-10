namespace HelloWorld
{
  public partial class Form1 : Form
  {
    private string name1;
    private string name2;

    public Form1()
    {
      InitializeComponent();
      name1 = "John";
      name2 = "Jill";
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      label1.Text = Resources.HelloWorld;
    }

    private void button1_Click(object sender, EventArgs e)
    {
      label1.Text = String.Format(Resources.HelloNames, name1, name2);
    }
  }
}
