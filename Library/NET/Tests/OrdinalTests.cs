using System;
using System.Globalization;
using System.Threading;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Soluling;

namespace Tests
{
  [TestClass]
  public class OrdinalTests
  {
    private void Test(
      string result, 
      OrdinalStringForm form, 
      uint ordinal, 
      Plural plural = Plural.One, 
      Gender gender = Gender.Neutral)
    {
      var str = Ordinal.Format(form, ordinal, plural, gender);
      Assert.IsTrue(result == str);
    }

    private void SingularShort(uint ordinal, string result)
    {
      Test(result, OrdinalStringForm.Short, ordinal);
    }

    private void SingularLong(uint ordinal, string result)
    {
      Test(result, OrdinalStringForm.Long, ordinal);
    }

    private void PluralLong(uint ordinal, string result)
    {
      Test(result, OrdinalStringForm.Long, ordinal, Plural.Other);
    }

    [TestMethod]
    [TestCategory("Ordinal")]
    public void English()
    {
      Language.Id = "en";

      SingularShort(1, "1st");
      SingularShort(2, "2nd");
      SingularShort(3, "3rd");
      SingularShort(4, "4th");
      SingularShort(10, "10th");
      SingularShort(11, "11th");
      SingularShort(21, "21st");

      SingularLong(1, "first");
      SingularLong(2, "second");
      SingularLong(3, "third");
      SingularLong(4, "fourth");
      SingularLong(10, "tenth");
      SingularLong(11, "11th");
      SingularLong(21, "21st");
    }

    [TestMethod]
    [TestCategory("Ordinal")]
    public void FinnishSingular()
    {
      Language.Id = "fi";

      SingularShort(1, "1.");
      SingularShort(2, "2.");
      SingularShort(3, "3.");
      SingularShort(4, "4.");
      SingularShort(10, "10.");
      SingularShort(11, "11.");
      SingularShort(21, "21.");

      SingularLong(1, "ensimmäinen");
      SingularLong(2, "toinen");
      SingularLong(3, "kolmas");
      SingularLong(4, "neljäs");
      SingularLong(10, "kymmenes");
      SingularLong(11, "11.");
      SingularLong(21, "21.");
    }

    [TestMethod]
    [TestCategory("Ordinal")]
    public void FinnishPlural()
    {
      Language.Id = "fi";

      PluralLong(1, "ensimmäiset");
      PluralLong(2, "toiset");
      PluralLong(3, "kolmannet");
      PluralLong(4, "neljännet");
      PluralLong(10, "kymmenennet");
      PluralLong(11, "11.");
      PluralLong(21, "21.");
    }
  }
}
