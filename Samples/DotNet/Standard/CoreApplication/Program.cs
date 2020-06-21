using System;
using System.Globalization;

namespace CoreApplication
{
  class Program
  {
    static void Main(string[] args)
    {
      if (args.Length > 0)
        CultureInfo.CurrentCulture = new CultureInfo(args[0]);

      CultureInfo.CurrentUICulture =  CultureInfo.CurrentCulture;

      Console.WriteLine(Library.SampleClass.GetHello());
      Console.WriteLine(Library.SampleClass.GetSample());
      Console.WriteLine(Properties.Resources.Sample);
    }
  }
}
