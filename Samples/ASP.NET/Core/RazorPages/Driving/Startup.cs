using System.Reflection;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Razor;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Soluling.AspNet;

namespace RazorDriving
{
  public class Startup
  {
    public Startup(IConfiguration configuration)
    {
      Configuration = configuration;
    }

    public IConfiguration Configuration { get; }

    // This method gets called by the runtime. Use this method to add services to the container.
    public void ConfigureServices(IServiceCollection services)
    {
      services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });

      // Configure full MVC instead of RazorPages (AddMvc vs. AddRazorPages) here because we want to use controllers too. 
      //services.AddRazorPages()
      services.AddMvc(options => options.EnableEndpointRouting = false)
        .AddViewLocalization(LanguageViewLocationExpanderFormat.Suffix)
        .AddDataAnnotationsLocalization();
    }

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
    {
      if (env.IsDevelopment())
      {
        app.UseDeveloperExceptionPage();
      }
      else
      {
        app.UseExceptionHandler("/Error");
        // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
        app.UseHsts();
      }

      app.UseRequestLocalizationWithAvailableLanguages(Assembly.GetExecutingAssembly().Location, "en");

      app.UseStaticFiles();

      // Configure full MVC instead of RazorPages (UseMvc vs. UseEndpoints) here because we want to use controllers too. 
      app.UseMvc();
/*
      app.UseRouting();

      app.UseEndpoints(endpoints =>
      {
        endpoints.MapRazorPages();
      });
*/
    }
  }
}
