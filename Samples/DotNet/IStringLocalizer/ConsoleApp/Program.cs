using System;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Localization;

// https://mcguirev10.com/2018/01/31/net-core-class-library-dependency-injection.html

// https://keestalkstech.com/2018/04/dependency-injection-with-ioptions-in-console-apps-in-net-core-2/

namespace ConsoleApp
{
  interface IService
  {
    void Process(string value);
  }

  class Service: IService
  {
    private IStringLocalizer _localizer;

    public Service() //IStringLocalizer<Service> localizer)
    {
      //_localizer = localizer;
    }

    public void Process(string value)
    {
      //Console.WriteLine(_localizer["Hello"]);
      Console.WriteLine(value);
    }
  }

  public class Program
  {
    static void Main()
    {
      var host = Host.CreateDefaultBuilder()
        .ConfigureServices((context, services) =>
        {
          services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });
          services.AddSingleton<IService, Service>();

          services.AddTransient<IMainService, MainService>();
        })
        .Build();

      var service = ActivatorUtilities.CreateInstance<IMainService>(host.Services);
      service.Run();
    }

/*
    static void Main()
    {
      var services = new ServiceCollection();
      services.AddLocalization(opts => { opts.ResourcesPath = "Resources"; });
      services.AddSingleton(typeof(IStringLocalizer<Service>), typeof(StringLocalizer<Service>));
      services.AddSingleton<IService, Service>();
      
      var serviceProvider = services.BuildServiceProvider();

      var service = serviceProvider.GetService<IService>();
      service.Process("This is a sample");
    }
*/
  }

  interface IMainService
  {
    void Run();
  }

  public class MainService: IMainService
  {
    private IService _service;

    public MainService() //(IService service)
    {
      //_service = service;
    }

    public void Run()
    {
      Console.WriteLine("This is a sample");
      //_service.Process("This is a sample");
    }
  }
}
