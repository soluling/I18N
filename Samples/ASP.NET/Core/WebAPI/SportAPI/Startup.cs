using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Localization;
using Microsoft.AspNetCore.Mvc.Formatters;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Serialization;
using SportAPI.Models;

namespace SportAPI
{
  public class Startup
  {
    public Startup(IConfiguration configuration)
    {
      Configuration = configuration;
    }

    public static IConfiguration Configuration { get; private set; }

    // This method gets called by the runtime. Use this method to add services to the container.
    public void ConfigureServices(IServiceCollection services)
    {
      // Add support for CORS (cross-origin resource sharing)
      services.AddCors(c =>
      {
        c.AddPolicy("ApiPolicy", builder => builder
          .AllowAnyOrigin()
          .AllowAnyMethod()
          .AllowAnyHeader()
          .SetPreflightMaxAge(TimeSpan.FromMinutes(60)));
      });

      // Configure the database
      services.AddDbContext<SportContext>(options => options.UseSqlServer(Configuration.GetConnectionString("SportContext")));

      // Set the location of the resource files
      services.AddLocalization(options => options.ResourcesPath = "Resources");

      // Add MVC suppport and configure JSON formating so the playload would be easier to read for humans
      services
        .AddControllers(options =>
        {
          options.OutputFormatters.Add(new HtmlOutputFormatter());
        })
        .AddDataAnnotationsLocalization()
        .AddNewtonsoftJson(options =>
        {
          options.SerializerSettings.Formatting = Formatting.Indented;
          options.SerializerSettings.NullValueHandling = NullValueHandling.Ignore;
          options.SerializerSettings.DefaultValueHandling = DefaultValueHandling.Ignore;
          options.SerializerSettings.ContractResolver = new CamelCasePropertyNamesContractResolver();
          options.SerializerSettings.Converters.Add(new StringEnumConverter { NamingStrategy = new DefaultNamingStrategy(), AllowIntegerValues = true });
        });
    }

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
    {
      // Get available languages from DB
      var dbCultures = new List<CultureInfo>();

      using (var serviceScope = app.ApplicationServices.GetService<IServiceScopeFactory>().CreateScope())
      {
        var context = serviceScope.ServiceProvider.GetRequiredService<SportContext>();
        context.Database.EnsureCreated();
        SeedData.Initialize(context);

        var languages = context.SportLanguage.Select(s => s.Language).Distinct();

        foreach (var language in languages)
          dbCultures.Add(new CultureInfo(language));
      }

      if (env.IsDevelopment())
        app.UseDeveloperExceptionPage();

      // Use db languages to set the available languages for localization
      var supportedCultures = dbCultures.ToArray();

      var options = new RequestLocalizationOptions
      {
        DefaultRequestCulture = new RequestCulture("en"),
        SupportedCultures = supportedCultures,
        SupportedUICultures = supportedCultures
      };

      app.UseRequestLocalization(options);

      app.UseRouting();
      app.UseCors("ApiPolicy");

      app.UseEndpoints(endpoints =>
      {
        endpoints.MapControllers();
      });
    }
  }

  public class HtmlOutputFormatter : StringOutputFormatter
  {
    public HtmlOutputFormatter()
    {
      SupportedMediaTypes.Add("text/html");
    }
  }
}