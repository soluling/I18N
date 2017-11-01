using System;

namespace WindowsForms
{
  public partial class InheritedForm : BaseForm
  {
    public InheritedForm()
    {
      InitializeComponent();
    }

    private void InheritedForm_Load(object sender, EventArgs e)
    {
      label1.Text = Properties.Resources.EnterValue;
    }
  }
}
