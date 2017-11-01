using System.Globalization;
using System.Threading;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewTool;

namespace Tests
{
  [TestClass]
  public class NumberTests
  {
    public int Precision { get; set; } = 2;

    public string Language
    {
      set
      {
        Thread.CurrentThread.CurrentCulture = new CultureInfo(value);
        Thread.CurrentThread.CurrentUICulture = new CultureInfo(value);
      }
    }

    private void Test(
      string result, 
      AbbreviatedNumberForm form, 
      double value)
    {
      var str = AbbreviatedNumber.Format(form, value, Precision);
      str = str.Replace((char)160, ' ');
      Assert.IsTrue(result == str);
    }

    private void Process(double value, string longResult, string shortResult, string currencyResult)
    {
      Test(longResult, AbbreviatedNumberForm.Long, value);
      Test(shortResult, AbbreviatedNumberForm.Short, value);
      Test(currencyResult, AbbreviatedNumberForm.Currency, value);
    }

    [TestMethod]
    [TestCategory("Number")]
    public void English()
    {
      Language = "en";

      Precision = 3;
      Process(1, "1", "1", "$1");
      Process(2.4, "2.4", "2.4", "$2.4");
      Process(121, "121", "121", "$121");
      Process(1000, "1 thousand", "1K", "$1K");
      Process(1650, "1.65 thousand", "1.65K", "$1.65K");
      Process(27450, "27.4 thousand", "27.4K", "$27.4K");
      Process(190394, "190 thousand", "190K", "$190K");
      Process(3400000, "3.4 million", "3.4M", "$3.4M");
      Process(54000000, "54 million", "54M", "$54M");
      Process(670000000, "670 million", "670M", "$670M");
      Process(1200000000, "1.2 billion", "1.2B", "$1.2B");
      Process(20200000000, "20.2 billion", "20.2B", "$20.2B");
      Process(410000000000, "410 billion", "410B", "$410B");
      Process(9520000000000, "9.52 trillion", "9.52T", "$9.52T");

      Precision = 2;
      Process(1, "1", "1", "$1");
      Process(2.4, "2.4", "2.4", "$2.4");
      Process(121, "120", "120", "$120");
      Process(1000, "1 thousand", "1K", "$1K");
      Process(1650, "1.6 thousand", "1.6K", "$1.6K");
      Process(27450, "27 thousand", "27K", "$27K");
      Process(190394, "190 thousand", "190K", "$190K");
      Process(3400000, "3.4 million", "3.4M", "$3.4M");
      Process(54000000, "54 million", "54M", "$54M");
      Process(670000000, "670 million", "670M", "$670M");
      Process(1200000000, "1.2 billion", "1.2B", "$1.2B");
      Process(20200000000, "20 billion", "20B", "$20B");
      Process(410000000000, "410 billion", "410B", "$410B");
      Process(9520000000000, "9.5 trillion", "9.5T", "$9.5T");

      Precision = 1;
      Process(1, "1", "1", "$1");
      Process(2.4, "2", "2", "$2");
      Process(121, "100", "100", "$100");
      Process(1000, "1 thousand", "1K", "$1K");
      Process(1650, "2 thousand", "2K", "$2K");
      Process(27450, "30 thousand", "30K", "$30K");
      Process(190394, "200 thousand", "200K", "$200K");
      Process(3400000, "3 million", "3M", "$3M");
      Process(54000000, "50 million", "50M", "$50M");
      Process(670000000, "700 million", "700M", "$700M");
      Process(1200000000, "1 billion", "1B", "$1B");
      Process(20200000000, "20 billion", "20B", "$20B");
      Process(410000000000, "400 billion", "400B", "$400B");
      Process(9520000000000, "10 trillion", "10T", "$10T");
    }

    [TestMethod]
    [TestCategory("Number")]
    public void Finnish()
    {
      Language = "fi";

      Precision = 3;
      Process(1, "1", "1", "1 €");
      Process(2.4, "2,4", "2,4", "2,4 €");
      Process(121, "121", "121", "121 €");
      Process(1000, "1 tuhat", "1 t.", "1 t. €");  // Singular
      Process(1650, "1,65 tuhatta", "1,65 t.", "1,65 t. €");
      Process(27450, "27,4 tuhatta", "27,4 t.", "27,4 t. €");
      Process(190394, "190 tuhatta", "190 t.", "190 t. €");
      Process(3400000, "3,4 miljoonaa", "3,4 milj.", "3,4 milj. €");
      Process(54000000, "54 miljoonaa", "54 milj.", "54 milj. €");
      Process(670000000, "670 miljoonaa", "670 milj.", "670 milj. €");
      Process(1200000000, "1,2 miljardia", "1,2 mrd.", "1,2 mrd. €");
      Process(20200000000, "20,2 miljardia", "20,2 mrd.", "20,2 mrd. €");
      Process(410000000000, "410 miljardia", "410 mrd.", "410 mrd. €");
      Process(9520000000000, "9,52 biljoonaa", "9,52 bilj.", "9,52 bilj. €");

      Precision = 2;
      Process(1, "1", "1", "1 €");
      Process(2.4, "2,4", "2,4", "2,4 €");
      Process(121, "120", "120", "120 €");
      Process(1000, "1 tuhat", "1 t.", "1 t. €");  // Singular
      Process(1650, "1,6 tuhatta", "1,6 t.", "1,6 t. €");
      Process(27450, "27 tuhatta", "27 t.", "27 t. €");
      Process(190394, "190 tuhatta", "190 t.", "190 t. €");
      Process(3400000, "3,4 miljoonaa", "3,4 milj.", "3,4 milj. €");
      Process(54000000, "54 miljoonaa", "54 milj.", "54 milj. €");
      Process(670000000, "670 miljoonaa", "670 milj.", "670 milj. €");
      Process(1200000000, "1,2 miljardia", "1,2 mrd.", "1,2 mrd. €");
      Process(20200000000, "20 miljardia", "20 mrd.", "20 mrd. €");
      Process(410000000000, "410 miljardia", "410 mrd.", "410 mrd. €");
      Process(9520000000000, "9,5 biljoonaa", "9,5 bilj.", "9,5 bilj. €");

      Precision = 1;
      Process(1, "1", "1", "1 €");
      Process(2.4, "2", "2", "2 €");
      Process(121, "100", "100", "100 €");
      Process(1000, "1 tuhat", "1 t.", "1 t. €");  // Singular
      Process(1650, "2 tuhatta", "2 t.", "2 t. €");
      Process(27450, "30 tuhatta", "30 t.", "30 t. €");
      Process(190394, "200 tuhatta", "200 t.", "200 t. €");
      Process(3400000, "3 miljoonaa", "3 milj.", "3 milj. €");
      Process(54000000, "50 miljoonaa", "50 milj.", "50 milj. €");
      Process(670000000, "700 miljoonaa", "700 milj.", "700 milj. €");
      Process(1200000000, "1 miljardi", "1 mrd.", "1 mrd. €");  // Singular
      Process(20200000000, "20 miljardia", "20 mrd.", "20 mrd. €");
      Process(410000000000, "400 miljardia", "400 mrd.", "400 mrd. €");
      Process(9520000000000, "10 biljoonaa", "10 bilj.", "10 bilj. €");
    }

    [TestMethod]
    [TestCategory("Number")]
    public void Swedish()
    {
      Language = "sv";

      Precision = 3;
      Process(1, "1", "1", "1 kr");
      Process(2.4, "2,4", "2,4", "2,4 kr");
      Process(121, "121", "121", "121 kr");
      Process(1000, "1 tusen", "1 tn", "1 tn kr");
      Process(1650, "1,65 tusen", "1,65 tn", "1,65 tn kr");
      Process(27450, "27,4 tusen", "27,4 tn", "27,4 tn kr");
      Process(190394, "190 tusen", "190 tn", "190 tn kr");
      Process(3400000, "3,4 miljoner", "3,4 mn", "3,4 mn kr");
      Process(54000000, "54 miljoner", "54 mn", "54 mn kr");
      Process(670000000, "670 miljoner", "670 mn", "670 mn kr");
      Process(1200000000, "1,2 miljarder", "1,2 md", "1,2 md kr");
      Process(20200000000, "20,2 miljarder", "20,2 md", "20,2 md kr");
      Process(410000000000, "410 miljarder", "410 md", "410 md kr");
      Process(9520000000000, "9,52 biljoner", "9,52 bn", "9,52 bn kr");

      Precision = 2;
      Process(1, "1", "1", "1 kr");
      Process(2.4, "2,4", "2,4", "2,4 kr");
      Process(121, "120", "120", "120 kr");
      Process(1000, "1 tusen", "1 tn", "1 tn kr");
      Process(1650, "1,6 tusen", "1,6 tn", "1,6 tn kr");
      Process(27450, "27 tusen", "27 tn", "27 tn kr");
      Process(190394, "190 tusen", "190 tn", "190 tn kr");
      Process(3400000, "3,4 miljoner", "3,4 mn", "3,4 mn kr");
      Process(54000000, "54 miljoner", "54 mn", "54 mn kr");
      Process(670000000, "670 miljoner", "670 mn", "670 mn kr");
      Process(1200000000, "1,2 miljarder", "1,2 md", "1,2 md kr");
      Process(20200000000, "20 miljarder", "20 md", "20 md kr");
      Process(410000000000, "410 miljarder", "410 md", "410 md kr");
      Process(9520000000000, "9,5 biljoner", "9,5 bn", "9,5 bn kr");

      Precision = 1;
      Process(1, "1", "1", "1 kr");
      Process(2.4, "2", "2", "2 kr");
      Process(121, "100", "100", "100 kr");
      Process(1000, "1 tusen", "1 tn", "1 tn kr");
      Process(1650, "2 tusen", "2 tn", "2 tn kr");
      Process(27450, "30 tusen", "30 tn", "30 tn kr");
      Process(190394, "200 tusen", "200 tn", "200 tn kr");
      Process(3400000, "3 miljoner", "3 mn", "3 mn kr");
      Process(54000000, "50 miljoner", "50 mn", "50 mn kr");
      Process(670000000, "700 miljoner", "700 mn", "700 mn kr");
      Process(1200000000, "1 miljard", "1 md", "1 md kr");  // Singular
      Process(20200000000, "20 miljarder", "20 md", "20 md kr");
      Process(410000000000, "400 miljarder", "400 md", "400 md kr");
      Process(9520000000000, "10 biljoner", "10 bn", "10 bn kr");
    }

    [TestMethod]
    [TestCategory("Number")]
    public void Japanese()
    {
      Language = "ja";

      Precision = 3;
      Process(1, "1", "1", "¥1");
      Process(2.4, "2.4", "2.4", "¥2.4");
      Process(121, "121", "121", "¥121");
      Process(1000, "1000", "1000", "¥1,000");
      Process(1650, "1650", "1650", "¥1,650");
      Process(27450, "2.74万", "2.74万", "¥2.74万");
      Process(190394, "19万", "19万", "¥19万");
      Process(3400000, "340万", "340万", "¥340万");
      Process(54000000, "5400万", "5400万", "¥5400万");
      Process(670000000, "6.7億", "6.7億", "¥6.7億");
      Process(1200000000, "12億", "12億", "¥12億");
      Process(20200000000, "202億", "202億", "¥202億");
      Process(410000000000, "4100億", "4100億", "¥4100億");
      Process(9520000000000, "9.52兆", "9.52兆", "¥9.52兆");

      Precision = 2;
      Process(1, "1", "1", "¥1");
      Process(2.4, "2.4", "2.4", "¥2.4");
      Process(121, "120", "120", "¥120");
      Process(1000, "1000", "1000", "¥1,000");
      Process(1650, "1600", "1600", "¥1,600");
      Process(27450, "2.7万", "2.7万", "¥2.7万");
      Process(190394, "19万", "19万", "¥19万");
      Process(3400000, "340万", "340万", "¥340万");
      Process(54000000, "5400万", "5400万", "¥5400万");
      Process(670000000, "6.7億", "6.7億", "¥6.7億");
      Process(1200000000, "12億", "12億", "¥12億");
      Process(20200000000, "200億", "200億", "¥200億");
      Process(410000000000, "4100億", "4100億", "¥4100億");
      Process(9520000000000, "9.5兆", "9.5兆", "¥9.5兆");

      Precision = 1;
      Process(1, "1", "1", "¥1");
      Process(2.4, "2", "2", "¥2");
      Process(121, "100", "100", "¥100");
      Process(1000, "1000", "1000", "¥1,000");
      Process(1650, "2000", "2000", "¥2,000");
      Process(27450, "3万", "3万", "¥3万");
      Process(190394, "20万", "20万", "¥20万");
      Process(3400000, "300万", "300万", "¥300万");
      Process(54000000, "5000万", "5000万", "¥5000万");
      Process(670000000, "7億", "7億", "¥7億");
      Process(1200000000, "10億", "10億", "¥10億");
      Process(20200000000, "200億", "200億", "¥200億");
      Process(410000000000, "4000億", "4000億", "¥4000億");
      Process(9520000000000, "10兆", "10兆", "¥10兆");
    }
  }
}
