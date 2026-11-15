using Simple.Resources.Strings;

namespace Simple;

public partial class MainPage : ContentPage
{
	int count = 0;

	public MainPage()
	{
		InitializeComponent();
	}

	private void OnCounterClicked(object sender, EventArgs e)
	{
		count++;

		// Do not concatenate strings. Use a format string, so the translator can change the word order.
		if (count == 1)
			CounterBtn.Text = String.Format(AppResources.ClickedSingular, count);
		else
			CounterBtn.Text = String.Format(AppResources.ClickedPlural, count);

		SemanticScreenReader.Announce(CounterBtn.Text);
	}
}
