using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace Resource
{
  public partial class Index : System.Web.UI.Page
  {
    protected void Page_Load(object sender, EventArgs e)
    {
      localize1.Text = Resources.Resource1.One;
      localize2.Text = GetGlobalResourceObject("Resource1", "One").ToString();
      
      localize3.Text = GetLocalResourceObject("One").ToString();

      localize4.Text = Resource1.One;
    }
  }
}