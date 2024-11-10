using System.Globalization;

namespace HelloWorld
{
  internal static class Program
  {
    /// <summary>
    ///  The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main()
    {
      Thread.CurrentThread.CurrentUICulture = CultureInfo.CurrentCulture;
      Thread.CurrentThread.CurrentCulture = CultureInfo.CurrentCulture;

      ApplicationConfiguration.Initialize();
      Application.Run(new Form1());
    }
  }
}