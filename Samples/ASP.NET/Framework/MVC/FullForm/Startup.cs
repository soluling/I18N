using Microsoft.Owin;
using Owin;

[assembly: OwinStartupAttribute(typeof(FullForm.Startup))]
namespace FullForm
{
    public partial class Startup
    {
        public void Configuration(IAppBuilder app)
        {
            //ConfigureAuth(app);
        }
    }
}
