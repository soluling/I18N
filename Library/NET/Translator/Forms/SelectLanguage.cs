using System.Globalization;
using System.Threading;
using System.Windows.Forms;

namespace Soluling.Forms
{
  /// <summary>
  /// Shows a dialog that lets the user to select a new active language. Once selected translates the application into that language.
  /// </summary>
  /// <remarks>
  /// This dialog provides the standard way to select a new language and to translate your application into the selected language. However you don't have
  /// to use this dialog. You can use the lower lever functions in the <see cref="T:Soluling.Language"/> class to build your own UI to show and select the language and
  /// once selected you can use the <see cref="T:Soluling.Forms.Translator"/> class to set the language of your application into the selected language.
  /// </remarks>
  /// <example>
  /// <para>
  /// The following example shows the language dialog when user clicks the language button.
  /// </para>
  /// <code>
  /// private void languageButton_Click(object sender, EventArgs e)
  /// {
  ///   if (SelectLanguage.Select())
  ///     UpdateItems();
  /// }    
  /// </code>
  /// <para>
  /// SelectLanguage.Select automatically translates all forms into the selected language. However it does not translate the properties that you have set on runtime. 
  /// You need to reset those properties by calling UpdateItems function. It is a method that you have to write and it sets values that you plan to set on runtime.
  /// UpdateItems will be called in the constructor to set the properties in the initial value using the default language. 
  /// The function is also called each time the language have been changed.
  /// </para>
  /// <code>
  /// public Form1()
  /// {
  ///   InitializeComponent();
  /// }
  /// 
  /// private void UpdateItems()
  /// {
  ///   label2.Text = Properties.Resources.String1;
  /// }
  /// </code>
  /// <para>
  /// If you don't set any properties on runtime the language change gets even simpler and you will need only one line of code.
  /// </para>
  /// <code>
  /// private void languageButton_Click(object sender, EventArgs e)
  /// {
  ///   SelectLanguage.Select();
  /// }    
  /// </code>
  /// </example>
  public partial class SelectLanguage : Form
  {
    private CultureInfo[] cultureInfos = null;
    private LanguageName displayLanguage = LanguageName.Both;

    /// <summary>
    /// Initializes a new instance of the SelectLanguage class.
    /// </summary>
    public SelectLanguage()
    {
      InitializeComponent();
      Populate(Language.OriginalId);
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
      listBox.Items.Add(Language.GetLanguageDisplayName(cultureInfo, DisplayLanguage));
    }

    private void Populate(string originalLanguage)
    {
      CultureInfo[] languages = Language.GetAvailableLanguages();

      cultureInfos = new CultureInfo[languages.Length + 1];
      listBox.BeginUpdate();
      AddLanguage(new CultureInfo(originalLanguage));

      for (int i = 0; i < languages.Length; i++)
        AddLanguage(languages[i]);

      listBox.EndUpdate();

      SelectedLanguage = Thread.CurrentThread.CurrentUICulture;
    }

    private void listBox_DoubleClick(object sender, System.EventArgs e)
    {
      DialogResult = DialogResult.OK;
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
    /// private void languageButton_Click(object sender, EventArgs e)
    /// {
    ///   SelectLanguage.Select();
    /// }    
    /// </code>
    /// </example>
    static public bool Select(LanguageName displayLanguage = LanguageName.Both)
    {
      SelectLanguage dialog = new SelectLanguage();
      dialog.DisplayLanguage = displayLanguage;

      if (dialog.ShowDialog() == DialogResult.OK)
      {
        Translator.SetUserInterfaceLanguage(dialog.SelectedLanguage);
        return true;
      }
      else
        return false;
    }
  }
}