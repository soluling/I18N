using System;
using System.Globalization;
using System.Threading;

namespace FrameworkApplication
{
  class Program
  {
    static void Main(string[] args)
    {
      if (args.Length > 0)
        Thread.CurrentThread.CurrentCulture = new CultureInfo(args[0]);

      Thread.CurrentThread.CurrentUICulture =  Thread.CurrentThread.CurrentCulture;

      Console.WriteLine(Library.SampleClass.GetHello());
      Console.WriteLine(Library.SampleClass.GetSample());
      Console.WriteLine(Properties.Resources.Sample);
    }
  }
}
