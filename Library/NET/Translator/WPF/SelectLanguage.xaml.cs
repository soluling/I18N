using System.Globalization;
using System.Threading;
using System.Windows;

namespace Soluling.WPF
{
  /// <summary>
  /// Shows a dialog that lets the user to select a new active language. Once selected translates the application into that language.
  /// </summary>
  public partial class SelectLanguage : Window
  {
    private CultureInfo[] cultureInfos = null;
    private LanguageName displayLanguage = LanguageName.Both;

    /// <summary>
    /// Initializes a new instance of the SelectLanguage class.
    /// </summary>
    public SelectLanguage(string originalLanguage)
    {
      InitializeComponent();
      Populate(originalLanguage);
    }

    /// <summary>
    /// Gets and sets the type of display language.
    /// </summary>
    public LanguageName DisplayLanguage
    {
      get { return displayLanguage; }
      set { displayLanguage = value; }
    }

    /// <summary>
    /// Gets and sets the selected language.
    /// </summary>
    public CultureInfo SelectedLanguage
    {
      get
      {
        return cultureInfos[listBox.SelectedIndex];
      }
      set
      {
        for (int i = 0; i < cultureInfos.Length; i++)
        {
          if (cultureInfos[i] == value)
          {
            listBox.SelectedIndex = i;
            return;
          }
        }

        for (int i = 0; i < cultureInfos.Length; i++)
        {
          if (cultureInfos[i].TwoLetterISOLanguageName == value.TwoLetterISOLanguageName)
          {
            listBox.SelectedIndex = i;
            return;
          }
        }

        if (listBox.Items.Count > 0)
          listBox.SelectedIndex = 0;
      }
    }

    private void AddLanguage(CultureInfo cultureInfo)
    {
      cultureInfos[listBox.Items.Count] = cultureInfo;
      listBox.Items.Add(Soluling.Language.GetLanguageDisplayName(cultureInfo, DisplayLanguage));
    }

    private void Populate(string originalLanguage)
    {
      CultureInfo[] languages = Soluling.Language.GetAvailableLanguages();

      var len = languages.Length;

      if (originalLanguage != "")
        len++;
  
      cultureInfos = new CultureInfo[len];

      if (originalLanguage != "")
        AddLanguage(new CultureInfo(originalLanguage));

      for (int i = 0; i < languages.Length; i++)
        AddLanguage(languages[i]);

      SelectedLanguage = Thread.CurrentThread.CurrentUICulture;
    }

    private void listBox_MouseDoubleClick(object sender, System.Windows.Input.MouseButtonEventArgs e)
    {
      DialogResult = true;
    }

    /// <summary>
    /// Shows language dialog that users can use to select a new language. If the user selects a language and clicks OK the method translates all current user interface (e.g. all forms) to the new language.
    /// </summary>
    /// <param name="displayLanguage">The type of the display language.</param>
    /// <returns><b>true</b> is user has selected a new language. <b>false</b> is user has clicked Cancel button.</returns>
    /// <example>
    /// <para>
    /// The following example shows the language dialog when user clicks the language button.
    /// </para>
    /// <code>
    /// private void languageButton_Click(object sender, RoutedEventArgs e)
    /// {
    ///   SelectLanguage.Select();
    /// }    
    /// </code>
    /// </example>
    static public bool Select(LanguageName displayLanguage = LanguageName.Both)
    {
      return Select(Soluling.Language.DefaultOriginalId, displayLanguage);
    }

    /// <summary>
    /// Shows language dialog that users can use to select a new language. If the user selects a language and clicks OK the method translates all current user interface (e.g. all forms) to the new language.
    /// </summary>
    /// <param name="originalLanguage">Language used in the original application. If empty original language is not shown.</param>
    /// <param name="displayLanguage">The type of the display language.</param>
    /// <returns><b>true</b> is user has selected a new language. <b>false</b> is user has clicked Cancel button.</returns>
    /// <example>
    /// <para>
    /// The following example shows the language dialog when user clicks the language button.
    /// </para>
    /// <code>
    /// private void languageButton_Click(object sender, RoutedEventArgs e)
    /// {
    ///   SelectLanguage.Select("en");
    /// }    
    /// </code>
    /// </example>
    static public bool Select(string originalLanguage, LanguageName displayLanguage = LanguageName.Both)
    {
      SelectLanguage dialog = new SelectLanguage(originalLanguage);
      dialog.DisplayLanguage = displayLanguage;

      if (dialog.ShowDialog() == true)
      {
        Translator.SetUserInterfaceLanguage(dialog.SelectedLanguage);
        return true;
      }
      else
        return false;
    }

    private void okButton_Click(object sender, RoutedEventArgs e)
    {
      DialogResult = true;
    }
  }
}
