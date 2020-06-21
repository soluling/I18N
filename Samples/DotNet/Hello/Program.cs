using System;
using System.Globalization;

namespace Hello
{
  class Program
  {
    static void Main(string[] args)
    {
      if (args.Length > 0)
        CultureInfo.CurrentCulture = new CultureInfo(args[0]);

      CultureInfo.CurrentUICulture = CultureInfo.CurrentCulture;

      Console.WriteLine(Properties.Resources.HelloWorld);
      Console.WriteLine(Properties.Resources.Sample);
      Console.WriteLine(String.Format(Properties.Resources.Today, DateTime.Now.ToShortDateString()));
      Console.WriteLine(String.Format(Properties.Resources.Time, DateTime.Now.ToShortTimeString()));
    }
  }
}