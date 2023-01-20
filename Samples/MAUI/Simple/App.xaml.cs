using System.Globalization;

namespace Simple;

public partial class App : Application
{
	public App()
	{
    Thread.CurrentThread.CurrentUICulture = CultureInfo.CurrentCulture;
    Thread.CurrentThread.CurrentCulture = CultureInfo.CurrentCulture;
    
    InitializeComponent();

    MainPage = new AppShell();
	}
}
