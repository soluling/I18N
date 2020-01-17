using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Globalization;

namespace Plural
{
  public partial class Index : System.Web.UI.Page
  {
    private string Process(uint count)
    {
      return NewTool.Plural.Format(Resources.Resources.MessagePlural, count, count);
    }

    protected void Page_Load(object sender, EventArgs e)
    {
      NewTool.Plural.SetLanguage(Resources.Resources.Language);

      languageLabel.Text = new CultureInfo(Resources.Resources.Language).NativeName;

      label0.Text = Process(0);
      label1.Text = Process(1);
      label2.Text = Process(2);
      label3.Text = Process(3);
      label4.Text = Process(4);
      label5.Text = Process(5);
      label6.Text = Process(11);
      label7.Text = Process(21);
      label8.Text = Process(101);
      label9.Text = Process(111);
    }
  }
}