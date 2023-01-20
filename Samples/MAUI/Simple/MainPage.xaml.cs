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

		if (count == 1)
			CounterBtn.Text = String.Format(Resource1.ClickedSingular, count);
		else
      CounterBtn.Text = String.Format(Resource1.ClickedPlural, count);

		SemanticScreenReader.Announce(CounterBtn.Text);
	}
}

