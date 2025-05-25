namespace WinFormsApp
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      label1.Text = Resources.ThisIsASample;
      label2.Text = "John";  // Do not localize
      label3.Text = Resources.FirstLineSecondLine;
    }
  }
}
