namespace Simple;

public partial class App : Application
{
	public App()
	{
		// The application uses the language of the device. To use a specific language, set the UI culture
		// before any page is created, for example:
		//
		// var culture = new System.Globalization.CultureInfo("fi");
		// System.Globalization.CultureInfo.DefaultThreadCurrentUICulture = culture;
		// System.Globalization.CultureInfo.CurrentUICulture = culture;

		InitializeComponent();
	}

	protected override Window CreateWindow(IActivationState? activationState)
	{
		return new Window(new AppShell());
	}
}
