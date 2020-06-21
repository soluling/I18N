using System;
using System.Globalization;

namespace CustomNameApp
{
  class Program
  {
    static void Main(string[] args)
    {
      if (args.Length > 0)
        CultureInfo.CurrentCulture = new CultureInfo(args[0]);

      CultureInfo.CurrentUICulture = CultureInfo.CurrentCulture;

      Console.WriteLine(Properties.Resources.HelloWorld);
    }
  }
}
