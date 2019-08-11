using System.Globalization;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Soluling;

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

    private string language = "";

    private void Same(string value1, string value2)
    {
      Assert.IsTrue(value1 == value2);
    }

    private void Form(uint count, Plural plural, string id = "")
    {
      if (id != "")
        language = id;

      PluralProc func = MultiPattern.GetProc(new CultureInfo(language));

      Assert.IsTrue(func(count, (int)count, 0, 0, 0, 0) == plural);
    }

    private void Pattern(uint count, string format, string pattern, string id = "")
    {
      if (id != "")
        Language.Id = id;

      string thisPattern = MultiPattern.GetPattern(format, count);

      Assert.IsTrue(thisPattern == pattern);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void StandardIcu()
    {
       const string VALUE = "{file, plural one {{0} file} other {{0} files}}";

      Pattern(0, VALUE, "{0} files", "en");
      Pattern(1, VALUE, "{0} file");
      Pattern(2, VALUE, "{0} files");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void StandardIcuCompact()
    {
      const string VALUE = "{,plural one{{0} file}other{{0} files}}";

      Pattern(0, VALUE, "{0} files", "en");
      Pattern(1, VALUE, "{0} file");
      Pattern(2, VALUE, "{0} files");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void StandardLegacy()
    {
      const string VALUE = "one;{0} file;other;{0} files";

      Pattern(0, VALUE, "{0} files", "en");
      Pattern(1, VALUE, "{0} file");
      Pattern(2, VALUE, "{0} files");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void EscapeIcu()
    {
      const string VALUE = @"{car, plural, one {Item \{\\\} {0} car} other {Item \{\\\} {0} cars}}";

      Pattern(0, VALUE, @"Item {\} {0} cars", "en");
      Pattern(1, VALUE, @"Item {\} {0} car");
      Pattern(2, VALUE, @"Item {\} {0} cars");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void EscapeLegacy()
    {
      const string VALUE = "one;Item;;{0} car;other;Item;;{0} cars";

      Pattern(0, VALUE, "Item;{0} cars", "en");
      Pattern(1, VALUE, "Item;{0} car");
      Pattern(2, VALUE, "Item;{0} cars");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void CustomIcu()
    {
      const string VALUE = "{files, plural, zero {No files} one {One file} two {Two files} other {{0} files}}";

      Pattern(0, VALUE, "No files", "en");
      Pattern(1, VALUE, "One file");
      Pattern(2, VALUE, "Two files");
      Pattern(3, VALUE, "{0} files");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void CustomLegacy()
    {
      const string VALUE = "zero;No files;one;One file;two;Two files;other;{0} files";

      Pattern(0, VALUE, "No files", "en");
      Pattern(1, VALUE, "One file");
      Pattern(2, VALUE, "Two files");
      Pattern(3, VALUE, "{0} files");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void PluralsIcu()
    {
      const string PATTERN = "I have {file, plural =0 {no cars} one {{0} car} other {{0} cars}}.";

      Same(MultiPattern.Format(PATTERN, 0), "I have no cars.");
      Same(MultiPattern.Format(PATTERN, 1), "I have 1 car.");
      Same(MultiPattern.Format(PATTERN, 2), "I have 2 cars.");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void PluralsLegacy()
    {
      const string FORMAT = "zero;I have no cars;one;I have one car;other;I have {0} cars";

      Same(MultiPattern.Format(FORMAT, 0), "I have no cars");
      Same(MultiPattern.Format(FORMAT, 1), "I have one car");
      Same(MultiPattern.Format(FORMAT, 2), "I have 2 cars");
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MultiIcu()
    {
      const string MULTI = "{name, gender, male {{0} will bring his} female {{0} will bring her}} {cars, plural, one {car} other {{0} cars}}.";
      Language.Id = "en";

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

    [TestMethod]
    [TestCategory("Plural")]
    public void MultiLegacy()
    {
      const string MULTI = "male;{0} will bring his {1};female;{0} will bring her {1};next;one;car.;other;{0} cars.";
      Language.Id = "en";

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

    // XxxForm() test that the passed count uses the plural form

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
    public void EnglishIcu()
    {
      Pattern(0, Resources.OneOtherIcu, OTHER, "en");
      Pattern(1, Resources.OneOtherIcu, ONE);
      Pattern(2, Resources.OneOtherIcu, OTHER);
      Pattern(10, Resources.OneOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneOtherIcu, OTHER);
      Pattern(10, Resources.ZeroOneOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void EnglishLegacy()
    {
      Pattern(0, Resources.OneOtherLegacy, OTHER, "en");
      Pattern(1, Resources.OneOtherLegacy, ONE);
      Pattern(2, Resources.OneOtherLegacy, OTHER);
      Pattern(10, Resources.OneOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneOtherLegacy, OTHER);
      Pattern(10, Resources.ZeroOneOtherLegacy, OTHER);
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
    public void FrenchIcu()
    {
      Pattern(0, Resources.OneOtherIcu, ONE, "fr");
      Pattern(1, Resources.OneOtherIcu, ONE);
      Pattern(2, Resources.OneOtherIcu, OTHER);
      Pattern(10, Resources.OneOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneOtherIcu, OTHER);
      Pattern(10, Resources.ZeroOneOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void FrenchLegacy()
    {
      Pattern(0, Resources.OneOtherLegacy, ONE, "fr");
      Pattern(1, Resources.OneOtherLegacy, ONE);
      Pattern(2, Resources.OneOtherLegacy, OTHER);
      Pattern(10, Resources.OneOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneOtherLegacy, OTHER);
      Pattern(10, Resources.ZeroOneOtherLegacy, OTHER);
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
    public void JapaneseIcu()
    {
      Pattern(0, Resources.OtherIcu, OTHER, "ja");
      Pattern(1, Resources.OtherIcu, OTHER);
      Pattern(2, Resources.OtherIcu, OTHER);
      Pattern(10, Resources.OtherIcu, OTHER);

      Pattern(0, Resources.ZeroOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOtherIcu, OTHER);
      Pattern(2, Resources.ZeroOtherIcu, OTHER);
      Pattern(10, Resources.ZeroOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void JapaneseLegacy()
    {
      Pattern(0, Resources.OtherLegacy, OTHER, "ja");
      Pattern(1, Resources.OtherLegacy, OTHER);
      Pattern(2, Resources.OtherLegacy, OTHER);
      Pattern(10, Resources.OtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOtherLegacy, OTHER);
      Pattern(2, Resources.ZeroOtherLegacy, OTHER);
      Pattern(10, Resources.ZeroOtherLegacy, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RussianForm()
    {
      Form(0, Plural.Many, "ru");
      Form(1, Plural.One);
      Form(2, Plural.Few);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Many);
      Form(10, Plural.Many);

      Form(11, Plural.Many);
      Form(12, Plural.Many);
      Form(13, Plural.Many);
      Form(14, Plural.Many);
      Form(15, Plural.Many);

      Form(21, Plural.One);
      Form(22, Plural.Few);
      Form(23, Plural.Few);
      Form(24, Plural.Few);
      Form(25, Plural.Many);

      Form(101, Plural.One);
      Form(102, Plural.Few);
      Form(103, Plural.Few);
      Form(104, Plural.Few);
      Form(105, Plural.Many);

      Form(111, Plural.Many);
      Form(112, Plural.Many);
      Form(113, Plural.Many);
      Form(114, Plural.Many);
      Form(115, Plural.Many);

      Form(121, Plural.One);
      Form(122, Plural.Few);
      Form(123, Plural.Few);
      Form(124, Plural.Few);
      Form(125, Plural.Many);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RussianIcu()
    {
      Pattern(0, Resources.OneFewManyIcu, MANY, "ru");
      Pattern(1, Resources.OneFewManyIcu, ONE);
      Pattern(2, Resources.OneFewManyIcu, FEW);
      Pattern(10, Resources.OneFewManyIcu, MANY);

      Pattern(0, Resources.ZeroOneFewManyIcu, ZERO);
      Pattern(1, Resources.ZeroOneFewManyIcu, ONE);
      Pattern(2, Resources.ZeroOneFewManyIcu, FEW);
      Pattern(10, Resources.ZeroOneFewManyIcu, MANY);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RussianLegacy()
    {
      Pattern(0, Resources.OneFewManyLegacy, MANY, "ru");
      Pattern(1, Resources.OneFewManyLegacy, ONE);
      Pattern(2, Resources.OneFewManyLegacy, FEW);
      Pattern(10, Resources.OneFewManyLegacy, MANY);

      Pattern(0, Resources.ZeroOneFewManyLegacy, ZERO);
      Pattern(1, Resources.ZeroOneFewManyLegacy, ONE);
      Pattern(2, Resources.ZeroOneFewManyLegacy, FEW);
      Pattern(10, Resources.ZeroOneFewManyLegacy, MANY);
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
    public void CzechIcu()
    {
      Pattern(0, Resources.OneFewOtherIcu, OTHER, "cs");
      Pattern(1, Resources.OneFewOtherIcu, ONE);
      Pattern(2, Resources.OneFewOtherIcu, FEW);
      Pattern(10, Resources.OneFewOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneFewOtherIcu, FEW);
      Pattern(10, Resources.ZeroOneFewOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void CzechLegacy()
    {
      Pattern(0, Resources.OneFewOtherLegacy, OTHER, "cs");
      Pattern(1, Resources.OneFewOtherLegacy, ONE);
      Pattern(2, Resources.OneFewOtherLegacy, FEW);
      Pattern(10, Resources.OneFewOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneFewOtherLegacy, FEW);
      Pattern(10, Resources.ZeroOneFewOtherLegacy, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IrishForm()
    {
      Form(0, Plural.Other, "ga");
      Form(1, Plural.One);
      Form(2, Plural.Two);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Few);
      Form(10, Plural.Many);
      Form(20, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IrishIcu()
    {
      Pattern(0, Resources.OneTwoFewManyOtherIcu, OTHER, "ga");
      Pattern(1, Resources.OneTwoFewManyOtherIcu, ONE);
      Pattern(2, Resources.OneTwoFewManyOtherIcu, TWO);
      Pattern(3, Resources.OneTwoFewManyOtherIcu, FEW);
      Pattern(10, Resources.OneTwoFewManyOtherIcu, MANY);
      Pattern(20, Resources.OneTwoFewManyOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneTwoFewManyOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneTwoFewManyOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneTwoFewManyOtherIcu, TWO);
      Pattern(3, Resources.ZeroOneTwoFewManyOtherIcu, FEW);
      Pattern(10, Resources.ZeroOneTwoFewManyOtherIcu, MANY);
      Pattern(20, Resources.ZeroOneTwoFewManyOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IrishLegacy()
    {
      Pattern(0, Resources.OneTwoFewManyOtherLegacy, OTHER, "ga");
      Pattern(1, Resources.OneTwoFewManyOtherLegacy, ONE);
      Pattern(2, Resources.OneTwoFewManyOtherLegacy, TWO);
      Pattern(3, Resources.OneTwoFewManyOtherLegacy, FEW);
      Pattern(10, Resources.OneTwoFewManyOtherLegacy, MANY);
      Pattern(20, Resources.OneTwoFewManyOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneTwoFewManyOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneTwoFewManyOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneTwoFewManyOtherLegacy, TWO);
      Pattern(3, Resources.ZeroOneTwoFewManyOtherLegacy, FEW);
      Pattern(10, Resources.ZeroOneTwoFewManyOtherLegacy, MANY);
      Pattern(20, Resources.ZeroOneTwoFewManyOtherLegacy, OTHER);
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
    public void ArabicIcu()
    {
      Pattern(0, Resources.ZeroOneTwoFewManyOtherIcu, ZERO, "ar");
      Pattern(1, Resources.ZeroOneTwoFewManyOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneTwoFewManyOtherIcu, TWO);
      Pattern(3, Resources.ZeroOneTwoFewManyOtherIcu, FEW);
      Pattern(11, Resources.ZeroOneTwoFewManyOtherIcu, MANY);
      Pattern(100, Resources.ZeroOneTwoFewManyOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void ArabicLegacy()
    {
      Pattern(0, Resources.ZeroOneTwoFewManyOtherLegacy, ZERO, "ar");
      Pattern(1, Resources.ZeroOneTwoFewManyOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneTwoFewManyOtherLegacy, TWO);
      Pattern(3, Resources.ZeroOneTwoFewManyOtherLegacy, FEW);
      Pattern(11, Resources.ZeroOneTwoFewManyOtherLegacy, MANY);
      Pattern(100, Resources.ZeroOneTwoFewManyOtherLegacy, OTHER);
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
    public void IcelandicIcu()
    {
      Pattern(0, Resources.OneOtherIcu, OTHER, "is");
      Pattern(1, Resources.OneOtherIcu, ONE);
      Pattern(2, Resources.OneOtherIcu, OTHER);
      Pattern(10, Resources.OneOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneOtherIcu, OTHER);
      Pattern(10, Resources.ZeroOneOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void IcelandicLegacy()
    {
      Pattern(0, Resources.OneOtherLegacy, OTHER, "is");
      Pattern(1, Resources.OneOtherLegacy, ONE);
      Pattern(2, Resources.OneOtherLegacy, OTHER);
      Pattern(10, Resources.OneOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneOtherLegacy, OTHER);
      Pattern(10, Resources.ZeroOneOtherLegacy, OTHER);
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
      Form(111, Plural.Zero);
      Form(221, Plural.One);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LatvianIcu()
    {
      Pattern(0, Resources.ZeroOneOtherIcu, ZERO, "lv");
      Pattern(1, Resources.ZeroOneOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneOtherIcu, OTHER);
      Pattern(10, Resources.ZeroOneOtherIcu, ZERO);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LatvianLegacy()
    {
      Pattern(0, Resources.ZeroOneOtherLegacy, ZERO, "lv");
      Pattern(1, Resources.ZeroOneOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneOtherLegacy, OTHER);
      Pattern(10, Resources.ZeroOneOtherLegacy, ZERO);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LithuanianForm()
    {
      Form(0, Plural.Other, "lt");
      Form(1, Plural.One);
      Form(2, Plural.Few);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Few);
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
    public void LithuanianIcu()
    {
      Pattern(0, Resources.OneFewOtherIcu, OTHER, "lt");
      Pattern(1, Resources.OneFewOtherIcu, ONE);
      Pattern(2, Resources.OneFewOtherIcu, FEW);
      Pattern(10, Resources.OneFewOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneFewOtherIcu, FEW);
      Pattern(10, Resources.ZeroOneFewOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void LithuanianLegacy()
    {
      Pattern(0, Resources.OneFewOtherLegacy, OTHER, "lt");
      Pattern(1, Resources.OneFewOtherLegacy, ONE);
      Pattern(2, Resources.OneFewOtherLegacy, FEW);
      Pattern(10, Resources.OneFewOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneFewOtherLegacy, FEW);
      Pattern(10, Resources.ZeroOneFewOtherLegacy, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MacedonianForm()
    {
      Form(0, Plural.Other, "mk");
      Form(1, Plural.One);
      Form(2, Plural.Other);
      Form(3, Plural.Other);
      Form(4, Plural.Other);
      Form(5, Plural.Other);
      Form(10, Plural.Other);

      Form(11, Plural.One);
      Form(12, Plural.Other);
      Form(13, Plural.Other);

      Form(21, Plural.One);
      Form(22, Plural.Other);
      Form(23, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MacedonianIcu()
    {
      Pattern(0, Resources.OneTwoOtherIcu, OTHER, "mk");
      Pattern(1, Resources.OneTwoOtherIcu, ONE);
      Pattern(2, Resources.OneTwoOtherIcu, TWO);
      Pattern(10, Resources.OneTwoOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneTwoOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneTwoOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneTwoOtherIcu, TWO);
      Pattern(10, Resources.ZeroOneTwoOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MacedonianLegacy()
    {
      Pattern(0, Resources.OneTwoOtherLegacy, OTHER, "mk");
      Pattern(1, Resources.OneTwoOtherLegacy, ONE);
      Pattern(2, Resources.OneTwoOtherLegacy, TWO);
      Pattern(10, Resources.OneTwoOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneTwoOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneTwoOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneTwoOtherLegacy, TWO);
      Pattern(10, Resources.ZeroOneTwoOtherLegacy, OTHER);
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
    public void MalteseIcu()
    {
      Pattern(0, Resources.OneFewManyOtherIcu, FEW, "mt");
      Pattern(1, Resources.OneFewManyOtherIcu, ONE);
      Pattern(11, Resources.OneFewManyOtherIcu, MANY);
      Pattern(100, Resources.OneFewManyOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneFewManyOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneFewManyOtherIcu, ONE);
      Pattern(11, Resources.ZeroOneFewManyOtherIcu, MANY);
      Pattern(100, Resources.ZeroOneFewManyOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void MalteseLegacy()
    {
      Pattern(0, Resources.OneFewManyOtherLegacy, FEW, "mt");
      Pattern(1, Resources.OneFewManyOtherLegacy, ONE);
      Pattern(11, Resources.OneFewManyOtherLegacy, MANY);
      Pattern(100, Resources.OneFewManyOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneFewManyOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneFewManyOtherLegacy, ONE);
      Pattern(11, Resources.ZeroOneFewManyOtherLegacy, MANY);
      Pattern(100, Resources.ZeroOneFewManyOtherLegacy, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void PolishForm()
    {
      Form(0, Plural.Many, "pl");
      Form(1, Plural.One);
      Form(2, Plural.Few);
      Form(3, Plural.Few);
      Form(4, Plural.Few);
      Form(5, Plural.Many);
      Form(10, Plural.Many);

      Form(11, Plural.Many);
      Form(12, Plural.Many);
      Form(13, Plural.Many);
      Form(14, Plural.Many);
      Form(15, Plural.Many);

      Form(21, Plural.Many);
      Form(22, Plural.Few);
      Form(23, Plural.Few);
      Form(24, Plural.Few);
      Form(25, Plural.Many);

      Form(101, Plural.Many);
      Form(102, Plural.Few);
      Form(103, Plural.Few);
      Form(104, Plural.Few);
      Form(105, Plural.Many);

      Form(111, Plural.Many);
      Form(112, Plural.Many);
      Form(113, Plural.Many);
      Form(114, Plural.Many);
      Form(115, Plural.Many);

      Form(121, Plural.Many);
      Form(122, Plural.Few);
      Form(123, Plural.Few);
      Form(124, Plural.Few);
      Form(125, Plural.Many);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void PolishIcu()
    {
      Pattern(0, Resources.OneFewManyIcu, MANY, "pl");
      Pattern(1, Resources.OneFewManyIcu, ONE);
      Pattern(2, Resources.OneFewManyIcu, FEW);
      Pattern(10, Resources.OneFewManyIcu, MANY);

      Pattern(0, Resources.ZeroOneFewManyIcu, ZERO);
      Pattern(1, Resources.ZeroOneFewManyIcu, ONE);
      Pattern(2, Resources.ZeroOneFewManyIcu, FEW);
      Pattern(10, Resources.ZeroOneFewManyIcu, MANY);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void PolishLegacy()
    {
      Pattern(0, Resources.OneFewManyLegacy, MANY, "pl");
      Pattern(1, Resources.OneFewManyLegacy, ONE);
      Pattern(2, Resources.OneFewManyLegacy, FEW);
      Pattern(10, Resources.OneFewManyLegacy, MANY);

      Pattern(0, Resources.ZeroOneFewManyLegacy, ZERO);
      Pattern(1, Resources.ZeroOneFewManyLegacy, ONE);
      Pattern(2, Resources.ZeroOneFewManyLegacy, FEW);
      Pattern(10, Resources.ZeroOneFewManyLegacy, MANY);
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
      Form(20, Plural.Other);

      Form(21, Plural.Other);

      Form(110, Plural.Few);
      Form(111, Plural.Few);
      Form(120, Plural.Other);
      Form(121, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RomanianIcu()
    {
      Pattern(0, Resources.OneFewOtherIcu, FEW, "ro");
      Pattern(1, Resources.OneFewOtherIcu, ONE);
      Pattern(2, Resources.OneFewOtherIcu, FEW);
      Pattern(21, Resources.OneFewOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneFewOtherIcu, FEW);
      Pattern(21, Resources.ZeroOneFewOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void RomanianLegacy()
    {
      Pattern(0, Resources.OneFewOtherLegacy, FEW, "ro");
      Pattern(1, Resources.OneFewOtherLegacy, ONE);
      Pattern(2, Resources.OneFewOtherLegacy, FEW);
      Pattern(21, Resources.OneFewOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneFewOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneFewOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneFewOtherLegacy, FEW);
      Pattern(21, Resources.ZeroOneFewOtherLegacy, OTHER);
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
    public void SlovenianIcu()
    {
      Pattern(0, Resources.OneTwoFewOtherIcu, OTHER, "sl");
      Pattern(1, Resources.OneTwoFewOtherIcu, ONE);
      Pattern(2, Resources.OneTwoFewOtherIcu, TWO);
      Pattern(3, Resources.OneTwoFewOtherIcu, FEW);
      Pattern(10, Resources.OneTwoFewOtherIcu, OTHER);

      Pattern(0, Resources.ZeroOneTwoFewOtherIcu, ZERO);
      Pattern(1, Resources.ZeroOneTwoFewOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneTwoFewOtherIcu, TWO);
      Pattern(3, Resources.ZeroOneTwoFewOtherIcu, FEW);
      Pattern(10, Resources.ZeroOneTwoFewOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void SlovenianLegacy()
    {
      Pattern(0, Resources.OneTwoFewOtherLegacy, OTHER, "sl");
      Pattern(1, Resources.OneTwoFewOtherLegacy, ONE);
      Pattern(2, Resources.OneTwoFewOtherLegacy, TWO);
      Pattern(3, Resources.OneTwoFewOtherLegacy, FEW);
      Pattern(10, Resources.OneTwoFewOtherLegacy, OTHER);

      Pattern(0, Resources.ZeroOneTwoFewOtherLegacy, ZERO);
      Pattern(1, Resources.ZeroOneTwoFewOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneTwoFewOtherLegacy, TWO);
      Pattern(3, Resources.ZeroOneTwoFewOtherLegacy, FEW);
      Pattern(10, Resources.ZeroOneTwoFewOtherLegacy, OTHER);
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
    public void WelshIcu()
    {
      Pattern(0, Resources.ZeroOneTwoFewManyOtherIcu, ZERO, "cy");
      Pattern(1, Resources.ZeroOneTwoFewManyOtherIcu, ONE);
      Pattern(2, Resources.ZeroOneTwoFewManyOtherIcu, TWO);
      Pattern(3, Resources.ZeroOneTwoFewManyOtherIcu, FEW);
      Pattern(6, Resources.ZeroOneTwoFewManyOtherIcu, MANY);
      Pattern(10, Resources.ZeroOneTwoFewManyOtherIcu, OTHER);
    }

    [TestMethod]
    [TestCategory("Plural")]
    public void WelshLegacy()
    {
      Pattern(0, Resources.ZeroOneTwoFewManyOtherLegacy, ZERO, "cy");
      Pattern(1, Resources.ZeroOneTwoFewManyOtherLegacy, ONE);
      Pattern(2, Resources.ZeroOneTwoFewManyOtherLegacy, TWO);
      Pattern(3, Resources.ZeroOneTwoFewManyOtherLegacy, FEW);
      Pattern(6, Resources.ZeroOneTwoFewManyOtherLegacy, MANY);
      Pattern(10, Resources.ZeroOneTwoFewManyOtherLegacy, OTHER);
    }


    [TestMethod]
    [TestCategory("Operator")]
    public void OperatorIcu()
    {
      const string VALUE = "I have {file, plural =0 {no cars} =1 {one car} =2 {two cars} 3..4 {few cars} ~12 {dozen cars} other {{0} cars}}.";

      Same(MultiPattern.Format(VALUE, 0), "I have no cars.");
      Same(MultiPattern.Format(VALUE, 1), "I have one car.");
      Same(MultiPattern.Format(VALUE, 2), "I have two cars.");
      Same(MultiPattern.Format(VALUE, 3), "I have few cars.");
      Same(MultiPattern.Format(VALUE, 4), "I have few cars.");
      Same(MultiPattern.Format(VALUE, 5), "I have 5 cars.");
      Same(MultiPattern.Format(VALUE, 10), "I have 10 cars.");
      Same(MultiPattern.Format(VALUE, 11), "I have dozen cars.");
      Same(MultiPattern.Format(VALUE, 12), "I have dozen cars.");
      Same(MultiPattern.Format(VALUE, 13), "I have dozen cars.");
    }

    [TestMethod]
    [TestCategory("Operator")]
    public void OperatorLessThanIcu()
    {
      const string VALUE = "I have {file, plural <5 {a few cars} >=5 {many cars} >=10 {plenty of cars}}.";

      Same(MultiPattern.Format(VALUE, 0), "I have a few cars.");
      Same(MultiPattern.Format(VALUE, 1), "I have a few cars.");
      Same(MultiPattern.Format(VALUE, 2), "I have a few cars.");
      Same(MultiPattern.Format(VALUE, 3), "I have a few cars.");
      Same(MultiPattern.Format(VALUE, 4), "I have a few cars.");
      Same(MultiPattern.Format(VALUE, 5), "I have many cars.");
      Same(MultiPattern.Format(VALUE, 6), "I have many cars.");
      Same(MultiPattern.Format(VALUE, 10), "I have plenty of cars.");
      Same(MultiPattern.Format(VALUE, 11), "I have plenty of cars.");
    }


    [TestMethod]
    [TestCategory("Gender")]
    public void GendersIcu()
    {
      const string GENDER = "{name, gender, neutral {{0} will bring the car} male {{0} will bring his car} female {{0} will bring her car}}";

      Same(MultiPattern.Format(GENDER, Gender.Male, "John"), "John will bring his car");
      Same(MultiPattern.Format(GENDER, Gender.Female, "Jill"), "Jill will bring her car");
      Same(MultiPattern.Format(GENDER, Gender.Neutral, "Somebody"), "Somebody will bring the car");
    }

    [TestMethod]
    [TestCategory("Gender")]
    public void GendersLegacy()
    {
      const string GENDER = "neutral;{0} will bring the car;male;{0} will bring his car;female;{0} will bring her car";

      Same(MultiPattern.Format(GENDER, Gender.Male, "John"), "John will bring his car");
      Same(MultiPattern.Format(GENDER, Gender.Female, "Jill"), "Jill will bring her car");
      Same(MultiPattern.Format(GENDER, Gender.Neutral, "Somebody"), "Somebody will bring the car");
    }


    [TestMethod]
    [TestCategory("Select")]
    public void SelectIcu()
    {
      const string PATTERN = "I have {file, select car {cars} bike {bikes} ski {skis}}.";

      Same(MultiPattern.Format(PATTERN, "car"), "I have cars.");
      Same(MultiPattern.Format(PATTERN, "bike"), "I have bikes.");
      Same(MultiPattern.Format(PATTERN, "ski"), "I have skis.");
    }
  }
}
