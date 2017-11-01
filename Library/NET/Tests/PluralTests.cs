using System;
using System.Globalization;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewTool;

namespace Tests
{
  [TestClass]
  public class PluralGetKindTests
  {
    private const string ZERO = "No files";
    private const string ONE = "{0} file";
    private const string TWO = "Two files";
    private const string FEW = "Few files";
    private const string MANY = "Many files";
    private const string OTHER = "{0} files";

    private const string STANDARD = "one;{0} file;other;{0} files";
    private const string NUMBER = "=0;I have no card;=1;I have one car;=2;I have two cars;other;I have {0} cars";
    private const string ESCAPE = "one;Item;;{0} car;other;Item;;{0} cars";
    private const string CUSTOM = "zero;No files;one;One file;two;Two files;other;{0} files";

    private string language = "";

    private void Same(string value1, string value2)
    {
      Assert.IsTrue(value1 == value2);
    }

    private void Form(uint count, Plural plural, string id = "")
    {
      if (id != "")
        language = id;

      PluralIndexProc func = MultiPattern.GetIndexProc(new CultureInfo(language));

      Assert.IsTrue(func(count) == plural);
    }

    private void Pattern(uint count, string format, string pattern, string id = "")
    {
      if (id != "")
        MultiPattern.SetLanguage(id);

      string thisPattern = MultiPattern.GetPattern(format, count);

      Assert.IsTrue(thisPattern == pattern);
    }

    private void Kind(PluralGroup kind, string id)
    {
      Assert.IsTrue(MultiPattern.GetKind(id) == kind);
    }

    private void Kind(PluralGroup kind, string[] ids)
    {
      foreach (string id in ids)
        Assert.IsTrue(MultiPattern.GetKind(id) == kind);
    }

    private void KindNegative(PluralGroup kind, string id = "en")
    {
      Assert.IsFalse(MultiPattern.GetKind(id) == kind);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void Standard()
    {
      Pattern(0, STANDARD, "{0} files", "en");
      Pattern(1, STANDARD, "{0} file");
      Pattern(2, STANDARD, "{0} files");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void Escape()
    {
      Pattern(0, ESCAPE, "Item;{0} cars", "en");
      Pattern(1, ESCAPE, "Item;{0} car");
      Pattern(2, ESCAPE, "Item;{0} cars");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void Custom()
    {
      Pattern(0, CUSTOM, "No files", "en");
      Pattern(1, CUSTOM, "One file");
      Pattern(2, CUSTOM, "Two files");
      Pattern(3, CUSTOM, "{0} files");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void Formats()
    {
      const string FORMAT = "zero;I have no cars;one;I have one car;other;I have {0} cars";

      Same(MultiPattern.Format(FORMAT, 0), "I have no cars");
      Same(MultiPattern.Format(FORMAT, 1), "I have one car");
      Same(MultiPattern.Format(FORMAT, 2), "I have 2 cars");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void Genders()
    {
      const string GENDER = "neutral;{0} will bring the car;male;{0} will bring his car;female;{0} will bring her car";

      Same(MultiPattern.Format(GENDER, Gender.Male, "John"), "John will bring his car");
      Same(MultiPattern.Format(GENDER, Gender.Female, "Jill"), "Jill will bring her car");
      Same(MultiPattern.Format(GENDER, Gender.Neutral, "Somebody"), "Somebody will bring the car");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void Multi()
    {
      const string MULTI = "male;{0} will bring his {1};female;{0} will bring her {1};next;one;car.;other;{0} cars.";
      MultiPattern.SetLanguage("en");

      Same(
        MultiPattern.FormatMulti(
          MULTI,
          new
          {
            Gender = Gender.Male,
            Value = "John"
          },
          2),
        "John will bring his 2 cars.");

      Same(
        MultiPattern.FormatMulti(
          MULTI,
          new { Gender = Gender.Male, Value = "Bill" },
          1),
        "Bill will bring his car.");

      Same(
        MultiPattern.FormatMulti(
          MULTI,
          new { Gender = Gender.Female, Value = "Jill" },
          3),
        "Jill will bring her 3 cars.");

      Same(
        MultiPattern.FormatMulti(
          MULTI,
          new
          {
            Gender = Gender.Female,
            Value = "Alma"
          },
          1),
        "Alma will bring her car.");
    }

    // XxxKind() test that the languages in the passed language array use the specific plural kind
    // XxxForm() test that the passed count uses the plural form

    [TestMethod]
    [TestCategory("Plural")]
    public void EnglishKind()
    {
      Kind(PluralGroup.Default, MultiPattern.ENGLISH_KIND);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void EnglishForm()
    {
      Form(0, Plural.Other, "en");
      Form(1, Plural.One);
      Form(2, Plural.Other);
      Form(10, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void EnglishPattern()
    {
      Pattern(0, Resources.OneOtherPlural, OTHER, "en");
      Pattern(1, Resources.OneOtherPlural, ONE);
      Pattern(2, Resources.OneOtherPlural, OTHER);
      Pattern(10, Resources.OneOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneOtherPlural, OTHER);
      Pattern(10, Resources.ZeroOneOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void FrenchKind()
    {
      Kind(PluralGroup.French, MultiPattern.FRENCH_KIND);
      Kind(PluralGroup.French, "pt-BR");
      KindNegative(PluralGroup.French);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void FrenchForm()
    {
      Form(0, Plural.One, "fr");
      Form(1, Plural.One);
      Form(2, Plural.Other);
      Form(10, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void FrenchPattern()
    {
      Pattern(0, Resources.OneOtherPlural, ONE, "fr");
      Pattern(1, Resources.OneOtherPlural, ONE);
      Pattern(2, Resources.OneOtherPlural, OTHER);
      Pattern(10, Resources.OneOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneOtherPlural, OTHER);
      Pattern(10, Resources.ZeroOneOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void JapaneseKind()
    {
      Kind(PluralGroup.Japanese, MultiPattern.JAPANESE_KIND);
      KindNegative(PluralGroup.Japanese);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void JapaneseForm()
    {
      Form(0, Plural.Other, "ja");
      Form(1, Plural.Other);
      Form(2, Plural.Other);
      Form(10, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void JapanesePattern()
    {
      Pattern(0, Resources.OtherPlural, OTHER, "ja");
      Pattern(1, Resources.OtherPlural, OTHER);
      Pattern(2, Resources.OtherPlural, OTHER);
      Pattern(10, Resources.OtherPlural, OTHER);

      Pattern(0, Resources.ZeroOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOtherPlural, OTHER);
      Pattern(2, Resources.ZeroOtherPlural, OTHER);
      Pattern(10, Resources.ZeroOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RussianKind()
    {
      Kind(PluralGroup.Russian, MultiPattern.RUSSIAN_KIND);
      KindNegative(PluralGroup.Russian);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RussianForm()
    {
      Form(0, Plural.Other, "ru");
      Form(1, Plural.One);
      Form(2, Plural.Few);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Other);
      Form(10, Plural.Other);

      Form(11, Plural.Other);
      Form(12, Plural.Other);
      Form(13, Plural.Other);
      Form(14, Plural.Other);
      Form(15, Plural.Other);

      Form(21, Plural.One);
      Form(22, Plural.Few);
      Form(23, Plural.Few);
      Form(24, Plural.Few);
      Form(25, Plural.Other);

      Form(101, Plural.One);
      Form(102, Plural.Few);
      Form(103, Plural.Few);
      Form(104, Plural.Few);
      Form(105, Plural.Other);

      Form(111, Plural.Other);
      Form(112, Plural.Other);
      Form(113, Plural.Other);
      Form(114, Plural.Other);
      Form(115, Plural.Other);

      Form(121, Plural.One);
      Form(122, Plural.Few);
      Form(123, Plural.Few);
      Form(124, Plural.Few);
      Form(125, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RussianPattern()
    {
      Pattern(0, Resources.OneFewOtherPlural, OTHER, "ru");
      Pattern(1, Resources.OneFewOtherPlural, ONE);
      Pattern(2, Resources.OneFewOtherPlural, FEW);
      Pattern(10, Resources.OneFewOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneFewOtherPlural, FEW);
      Pattern(10, Resources.ZeroOneFewOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void CzechKind()
    {
      Kind(PluralGroup.Czech, MultiPattern.CZECH_KIND);
      KindNegative(PluralGroup.Czech);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void CzechForm()
    {
      Form(0, Plural.Other, "cs");
      Form(1, Plural.One);
      Form(2, Plural.Few);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Other);
      Form(10, Plural.Other);

      Form(11, Plural.Other);
      Form(12, Plural.Other);
      Form(13, Plural.Other);
      Form(14, Plural.Other);
      Form(15, Plural.Other);

      Form(101, Plural.Other);
      Form(102, Plural.Other);
      Form(103, Plural.Other);
      Form(104, Plural.Other);
      Form(105, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void CzechPattern()
    {
      Pattern(0, Resources.OneFewOtherPlural, OTHER, "cs");
      Pattern(1, Resources.OneFewOtherPlural, ONE);
      Pattern(2, Resources.OneFewOtherPlural, FEW);
      Pattern(10, Resources.OneFewOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneFewOtherPlural, FEW);
      Pattern(10, Resources.ZeroOneFewOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IrishKind()
    {
      Kind(PluralGroup.Irish, MultiPattern.IRISH_KIND);
      KindNegative(PluralGroup.Irish);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IrishForm()
    {
      Form(0, Plural.Other, "ga");
      Form(1, Plural.One);
      Form(2, Plural.Two);
      Form(3, Plural.Other);
      Form(4, Plural.Other);
      Form(5, Plural.Other);
      Form(10, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IrishPattern()
    {
      Pattern(0, Resources.OneTwoOtherPlural, OTHER, "ga");
      Pattern(1, Resources.OneTwoOtherPlural, ONE);
      Pattern(2, Resources.OneTwoOtherPlural, TWO);
      Pattern(10, Resources.OneTwoOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneTwoOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneTwoOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneTwoOtherPlural, TWO);
      Pattern(10, Resources.ZeroOneTwoOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void ArabicKind()
    {
      Kind(PluralGroup.Arabic, "ar");
      KindNegative(PluralGroup.Arabic);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void ArabicForm()
    {
      Form(0, Plural.Zero, "ar");
      Form(1, Plural.One);
      Form(2, Plural.Two);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Few);
      Form(10, Plural.Few);

      Form(11, Plural.Many);
      Form(20, Plural.Many);
      Form(99, Plural.Many);
      Form(100, Plural.Other);
      Form(101, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void ArabicPattern()
    {
      Pattern(0, Resources.ZeroOneTwoFewManyOtherPlural, ZERO, "ar");
      Pattern(1, Resources.ZeroOneTwoFewManyOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneTwoFewManyOtherPlural, TWO);
      Pattern(3, Resources.ZeroOneTwoFewManyOtherPlural, FEW);
      Pattern(11, Resources.ZeroOneTwoFewManyOtherPlural, MANY);
      Pattern(100, Resources.ZeroOneTwoFewManyOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IcelandicKind()
    {
      Kind(PluralGroup.Icelandic, "is");
      KindNegative(PluralGroup.Icelandic);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IcelandicForm()
    {
      Form(0, Plural.Other, "is");
      Form(1, Plural.One);
      Form(2, Plural.Other);
      Form(3, Plural.Other);
      Form(4, Plural.Other);
      Form(5, Plural.Other);
      Form(10, Plural.Other);

      Form(11, Plural.Other);
      Form(21, Plural.One);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IcelandicPattern()
    {
      Pattern(0, Resources.OneOtherPlural, OTHER, "is");
      Pattern(1, Resources.OneOtherPlural, ONE);
      Pattern(2, Resources.OneOtherPlural, OTHER);
      Pattern(10, Resources.OneOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneOtherPlural, OTHER);
      Pattern(10, Resources.ZeroOneOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LatvianKind()
    {
      Kind(PluralGroup.Latvian, "lv");
      KindNegative(PluralGroup.Latvian);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LatvianForm()
    {
      Form(0, Plural.Zero, "lv");
      Form(1, Plural.One);
      Form(2, Plural.Other);
      Form(3, Plural.Other);
      Form(4, Plural.Other);
      Form(5, Plural.Other);
      Form(10, Plural.Zero);

      Form(21, Plural.One);
      Form(31, Plural.One);
      Form(101, Plural.One);
      Form(111, Plural.Other);
      Form(221, Plural.One);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LatvianPattern()
    {
      Pattern(0, Resources.ZeroOneOtherPlural, ZERO, "lv");
      Pattern(1, Resources.ZeroOneOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneOtherPlural, OTHER);
      Pattern(10, Resources.ZeroOneOtherPlural, ZERO);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LithuanianKind()
    {
      Kind(PluralGroup.Lithuanian, "lt");
      KindNegative(PluralGroup.Lithuanian);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LithuanianForm()
    {
      Form(0, Plural.Other, "lt");
      Form(1, Plural.One);
      Form(2, Plural.Few);
      Form(3, Plural.Other);
      Form(4, Plural.Other);
      Form(5, Plural.Other);
      Form(10, Plural.Other);

      Form(11, Plural.Other);
      Form(21, Plural.One);
      Form(31, Plural.One);

      Form(12, Plural.Other);
      Form(22, Plural.Few);
      Form(32, Plural.Few);
      Form(102, Plural.Few);
      Form(112, Plural.Other);
      Form(222, Plural.Few);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LithuanianPattern()
    {
      Pattern(0, Resources.OneFewOtherPlural, OTHER, "lt");
      Pattern(1, Resources.OneFewOtherPlural, ONE);
      Pattern(2, Resources.OneFewOtherPlural, FEW);
      Pattern(10, Resources.OneFewOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneFewOtherPlural, FEW);
      Pattern(10, Resources.ZeroOneFewOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MacedonianKind()
    {
      Kind(PluralGroup.Macedonian, "mk");
      KindNegative(PluralGroup.Macedonian);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MacedonianForm()
    {
      Form(0, Plural.Other, "mk");
      Form(1, Plural.One);
      Form(2, Plural.Two);
      Form(3, Plural.Other);
      Form(4, Plural.Other);
      Form(5, Plural.Other);
      Form(10, Plural.Other);

      Form(11, Plural.One);
      Form(12, Plural.Two);
      Form(13, Plural.Other);

      Form(21, Plural.One);
      Form(22, Plural.Two);
      Form(23, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MacedonianPattern()
    {
      Pattern(0, Resources.OneTwoOtherPlural, OTHER, "mk");
      Pattern(1, Resources.OneTwoOtherPlural, ONE);
      Pattern(2, Resources.OneTwoOtherPlural, TWO);
      Pattern(10, Resources.OneTwoOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneTwoOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneTwoOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneTwoOtherPlural, TWO);
      Pattern(10, Resources.ZeroOneTwoOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MalteseKind()
    {
      Kind(PluralGroup.Maltese, "mt");
      KindNegative(PluralGroup.Maltese);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MalteseForm()
    {
      Form(0, Plural.Few, "mt");
      Form(1, Plural.One);
      Form(2, Plural.Few);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Few);
      Form(10, Plural.Few);

      Form(11, Plural.Many);
      Form(12, Plural.Many);
      Form(13, Plural.Many);
      Form(14, Plural.Many);
      Form(15, Plural.Many);
      Form(16, Plural.Many);
      Form(17, Plural.Many);
      Form(18, Plural.Many);
      Form(19, Plural.Many);
      Form(20, Plural.Other);

      Form(100, Plural.Other);
      Form(101, Plural.Other);
      Form(110, Plural.Few);
      Form(111, Plural.Many);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MaltesePattern()
    {
      Pattern(0, Resources.OneFewManyOtherPlural, FEW, "mt");
      Pattern(1, Resources.OneFewManyOtherPlural, ONE);
      Pattern(11, Resources.OneFewManyOtherPlural, MANY);
      Pattern(100, Resources.OneFewManyOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneFewManyOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneFewManyOtherPlural, ONE);
      Pattern(11, Resources.ZeroOneFewManyOtherPlural, MANY);
      Pattern(100, Resources.ZeroOneFewManyOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void PolishKind()
    {
      Kind(PluralGroup.Polish, "pl");
      KindNegative(PluralGroup.Polish);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void PolishForm()
    {
      Form(0, Plural.Other, "pl");
      Form(1, Plural.One);
      Form(2, Plural.Few);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Other);
      Form(10, Plural.Other);

      Form(11, Plural.Other);
      Form(12, Plural.Other);
      Form(13, Plural.Other);
      Form(14, Plural.Other);
      Form(15, Plural.Other);

      Form(21, Plural.Other);
      Form(22, Plural.Few);
      Form(23, Plural.Few);
      Form(24, Plural.Few);
      Form(25, Plural.Other);

      Form(101, Plural.Other);
      Form(102, Plural.Few);
      Form(103, Plural.Few);
      Form(104, Plural.Few);
      Form(105, Plural.Other);

      Form(111, Plural.Other);
      Form(112, Plural.Other);
      Form(113, Plural.Other);
      Form(114, Plural.Other);
      Form(115, Plural.Other);

      Form(121, Plural.Other);
      Form(122, Plural.Few);
      Form(123, Plural.Few);
      Form(124, Plural.Few);
      Form(125, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void PolishPattern()
    {
      Pattern(0, Resources.OneFewOtherPlural, OTHER, "pl");
      Pattern(1, Resources.OneFewOtherPlural, ONE);
      Pattern(2, Resources.OneFewOtherPlural, FEW);
      Pattern(10, Resources.OneFewOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneFewOtherPlural, FEW);
      Pattern(10, Resources.ZeroOneFewOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RomanianKind()
    {
      Kind(PluralGroup.Romanian, "ro");
      KindNegative(PluralGroup.Romanian);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RomanianForm()
    {
      Form(0, Plural.Few, "ro");
      Form(1, Plural.One);
      Form(2, Plural.Few);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Few);
      Form(10, Plural.Few);

      Form(11, Plural.Few);
      Form(12, Plural.Few);
      Form(13, Plural.Few);
      Form(14, Plural.Few);
      Form(15, Plural.Few);
      Form(16, Plural.Few);
      Form(17, Plural.Few);
      Form(18, Plural.Few);
      Form(19, Plural.Few);
      Form(20, Plural.Few);

      Form(21, Plural.Other);

      Form(110, Plural.Few);
      Form(111, Plural.Few);
      Form(120, Plural.Few);
      Form(121, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RomanianPattern()
    {
      Pattern(0, Resources.OneFewOtherPlural, FEW, "ro");
      Pattern(1, Resources.OneFewOtherPlural, ONE);
      Pattern(2, Resources.OneFewOtherPlural, FEW);
      Pattern(21, Resources.OneFewOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneFewOtherPlural, FEW);
      Pattern(21, Resources.ZeroOneFewOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void SlovenianKind()
    {
      Kind(PluralGroup.Slovenian, "sl");
      KindNegative(PluralGroup.Slovenian);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void SlovenianForm()
    {
      Form(0, Plural.Other, "sl");
      Form(1, Plural.One);
      Form(2, Plural.Two);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Other);

      Form(10, Plural.Other);
      Form(11, Plural.Other);
      Form(12, Plural.Other);
      Form(13, Plural.Other);
      Form(14, Plural.Other);
      Form(15, Plural.Other);

      Form(100, Plural.Other);
      Form(101, Plural.One);
      Form(102, Plural.Two);
      Form(103, Plural.Few);
      Form(104, Plural.Few);
      Form(105, Plural.Other);

      Form(110, Plural.Other);
      Form(111, Plural.Other);
      Form(112, Plural.Other);
      Form(113, Plural.Other);
      Form(114, Plural.Other);
      Form(115, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void SlovenianPattern()
    {
      Pattern(0, Resources.OneTwoFewOtherPlural, OTHER, "sl");
      Pattern(1, Resources.OneTwoFewOtherPlural, ONE);
      Pattern(2, Resources.OneTwoFewOtherPlural, TWO);
      Pattern(3, Resources.OneTwoFewOtherPlural, FEW);
      Pattern(10, Resources.OneTwoFewOtherPlural, OTHER);

      Pattern(0, Resources.ZeroOneTwoFewOtherPlural, ZERO);
      Pattern(1, Resources.ZeroOneTwoFewOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneTwoFewOtherPlural, TWO);
      Pattern(3, Resources.ZeroOneTwoFewOtherPlural, FEW);
      Pattern(10, Resources.ZeroOneTwoFewOtherPlural, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void WelshKind()
    {
      Kind(PluralGroup.Welsh, "cy");
      KindNegative(PluralGroup.Welsh);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void WelshForm()
    {
      Form(0, Plural.Zero, "cy");
      Form(1, Plural.One);
      Form(2, Plural.Two);
      Form(3, Plural.Few);
      Form(4, Plural.Other);
      Form(5, Plural.Other);
      Form(6, Plural.Many);
      Form(7, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void WelshPattern()
    {
      Pattern(0, Resources.ZeroOneTwoFewManyOtherPlural, ZERO, "cy");
      Pattern(1, Resources.ZeroOneTwoFewManyOtherPlural, ONE);
      Pattern(2, Resources.ZeroOneTwoFewManyOtherPlural, TWO);
      Pattern(3, Resources.ZeroOneTwoFewManyOtherPlural, FEW);
      Pattern(6, Resources.ZeroOneTwoFewManyOtherPlural, MANY);
      Pattern(10, Resources.ZeroOneTwoFewManyOtherPlural, OTHER);
    }
  }
}
