using System;
using System.Drawing;
using System.Windows.Forms;
using Soluling;

namespace MixedPlural
{
  public partial class Form1 : Form
  {
    public Form1()
    {
      InitializeComponent();
    }

    private void ProcessPluralUnaware(uint count, Label label)
    {
      // On most languages this does not work except when count is 1
      // Do not use code like this!
      label.Text = String.Format(Properties.Resources.File, count);

      if (MultiPattern.IsSingleFormLanguage() || (count == 1) || ((count == 0) && MultiPattern.IsZeroLikeOne()))
        label.ForeColor = Color.Green;
      else
        label.ForeColor = Color.Red;
    }

    private void ProcessHomeBrewed(uint count, Label label)
    {
      // This works on some Western languages (those that use similar plural rules as English)
      // but would fail for example in French.
      // Do not use code like this!
      if (count == 1)
        label.Text = String.Format(Properties.Resources.File, count);
      else
        label.Text = String.Format(Properties.Resources.Files, count);

      if (!MultiPattern.IsSingleFormLanguage() && (count == 0) && MultiPattern.IsZeroLikeOne())
        label.ForeColor = Color.Red;
      else
        label.ForeColor = Color.Green;
    }

    // The following two samples handle plural forms correctly. Use this kind of code in your applications.
    private void ProcessPluralAware(int count, Label label)
    {
      label.Text = MultiPattern.Format(Properties.Resources.FilesPlural, count, count);
      label.ForeColor = Color.Green;
    }

    private void ProcessMultiPlural(uint completed, int total, Label label)
    {
      label.Text = MultiPattern.FormatMulti(Properties.Resources.MessagePlural, completed, total);
      label.ForeColor = Color.Green;
    }

    private void Form1_Load(object sender, EventArgs e)
    {
      // Plural engine needs to know the language that the application uses.
      Language.Id = Properties.Resources.Language;

      ProcessPluralUnaware(0, noPluralLabel0);
      ProcessPluralUnaware(1, noPluralLabel1);
      ProcessPluralUnaware(2, noPluralLabel2);

      ProcessHomeBrewed(0, homeLabel0);
      ProcessHomeBrewed(1, homeLabel1);
      ProcessHomeBrewed(2, homeLabel2);

      ProcessPluralAware(0, pluralLabel0);
      ProcessPluralAware(1, pluralLabel1);
      ProcessPluralAware(2, pluralLabel2);

      ProcessMultiPlural(0, 1, multiPluralLabel1);
      ProcessMultiPlural(1, 1, multiPluralLabel2);
      ProcessMultiPlural(1, 3, multiPluralLabel3);
      ProcessMultiPlural(2, 3, multiPluralLabel4);
    }
  }
}
