using System.Collections.Generic;
using System.Globalization;

namespace NewTool
{
  /// <summary>
  /// Enumeration that specifies the string form of an ordinal.
  /// </summary>
  public enum OrdinalStringForm
  {
    /// <summary>Long string form such as "first" and "fourth"</summary>
    Long,

    /// <summary>Short string form such as "1st" and "4th"</summary>
    Short
  }

  /// <summary>
  /// Enumeration that specifies what plural group a language belongs to.
  /// </summary>
  /// <remarks>
  /// The algorithm is named based on the main language of the group.
  /// For example PluralGroup.Japanese is not only applied to Japanese but all the other languages where plural rules are the same as in Japanese (expecially Asian ones).
  /// </remarks>
  public enum SmallOrdinal
  {
    /// <summary>First</summary>
    First = 1,

    /// <summary>Second</summary>
    Second,

    /// <summary>Third</summary>
    Third,

    /// <summary>Fourth</summary>
    Fourth,

    /// <summary>Fifth</summary>
    Fifth,

    /// <summary>Sixth</summary>
    Sixth,

    /// <summary>Seventh</summary>
    Seventh,

    /// <summary>Eighth</summary>
    Eighth,

    /// <summary>Ninth</summary>
    Ninth,

    /// <summary>Tenth</summary>
    Tenth
  };

  /// <summary>
  /// Function that returns ordinal in a long form such as "first" and "fourth".
  /// </summary>
  /// <param name="ordinal">Ordinal value</param>
  /// <param name="plural">Plural form</param>
  /// <param name="gender">Gender</param>
  /// <returns>The ordinal in a long string form.</returns>
  public delegate string OrdinalLongProc(SmallOrdinal ordinal, Plural plural, Gender gender);

  /// <summary>
  /// Function that returns ordinal in a short form such as "1st" and "4th".
  /// </summary>
  /// <param name="ordinal">Ordinal value</param>
  /// <param name="plural">Plural form</param>
  /// <param name="gender">Gender</param>
  /// <returns>The ordinal in a short string form.</returns>
  public delegate string OrdinalShortProc(uint ordinal, Plural plural, Gender gender);

  class OrdinalData
  {
    public OrdinalLongProc LongProc { get; set; }
    public OrdinalShortProc ShortProc { get; set; }
  }

  /// <summary>
  /// Implements a formatting a number into its ordinal form.
  /// </summary>
  /// <remarks>
  /// For example in English "This is my 1st car" and "This is my 2nd card". Note the ending after the number.
  /// It is either st, nd, rd, or th depending on the number.
  /// Some other countries use different endings and the rules vary. Some countries just add pediod in the end
  /// like in Finnish "Tämä on minun 1. autoni" or "Tämä on minun 2. autoni".
  /// </remarks>
  public static class Ordinal
  {
    private static Dictionary<string, OrdinalData> datas = new Dictionary<string, OrdinalData>();

    /// <summary>
    /// Converts an ordinal number to a string such as "1st" or "first".
    /// </summary>
    /// <param name="form">String form to be used.</param>
    /// <param name="ordinal">Ordinal number.</param>
    /// <param name="plural">Specifies the plural form.</param>
    /// <param name="gender">Specifies the gender.</param>
    /// <returns>The formatted string.</returns>
    public static string Format(
      OrdinalStringForm form,
      uint ordinal, 
      Plural plural = Plural.One,
      Gender gender =  Gender.Neutral)
    {
      if (form == OrdinalStringForm.Short)
        return FormatShort(ordinal, plural, gender);
      else
        return FormatLong(ordinal, plural, gender);
    }

    /// <summary>
    /// Converts an ordinal number to a string using the short format such as "1st" and "4th".
    /// </summary>
    /// <param name="ordinal">Ordinal number.</param>
    /// <param name="plural">Specifies the plural form.</param>
    /// <param name="gender">Specifies the gender.</param>
    /// <returns>The formatted string.</returns>
    public static string FormatShort(
      uint ordinal, 
      Plural plural = Plural.One,
      Gender gender =  Gender.Neutral)
    {
      return GetData().ShortProc(ordinal, plural, gender);
    }

    /// <summary>
    /// Converts an ordinal number to a string using the long format such as "first" and "fourth".
    /// </summary>
    /// <remarks>
    /// Only the first 10 ordinals can be formatted to the long format. The rest of the ordinals will always
    /// be formatted using the short format such as "21st" and "24th".
    /// </remarks>
    /// <param name="ordinal">Ordinal number.</param>
    /// <param name="plural">Specifies the plural form.</param>
    /// <param name="gender">Specifies the gender.</param>
    /// <returns>The formatted string.</returns>
    public static string FormatLong(
      uint ordinal, 
      Plural plural = Plural.One,
      Gender gender =  Gender.Neutral)
    {
      if (IsShortOrdinal(ordinal))
        return GetData().LongProc((SmallOrdinal)ordinal, plural, gender);
      else
        return FormatShort(ordinal, plural, gender);
    }

    /// <summary>
    /// Register formatting functions for a language.
    /// </summary>
    /// <param name="id">ISO 639-1 language code of the language.</param>
    /// <param name="longProc">Function that converts a small ordianal to a long string.</param>
    /// <param name="shortProc">Function that converts an ordianal to a short string.</param>
    public static void Register(string id, OrdinalLongProc longProc, OrdinalShortProc shortProc)
    {
      OrdinalData data;

      if (!datas.TryGetValue(id, out data))
      {
        data = new OrdinalData();
        datas.Add(id, data);
      }

      data.LongProc = longProc;
      data.ShortProc = shortProc;
    }


    private static OrdinalData GetData()
    {
      var id = CultureInfo.CurrentUICulture.ToString();

      if (!datas.ContainsKey(id))
        id = "";

      return datas[id];
    }

    private static bool IsShortOrdinal(uint value)
    {
      return (value >= (uint)SmallOrdinal.First) && (value <= (uint)SmallOrdinal.Tenth);
    }


    private static string GetDefaultShort(uint ordinal, Plural plural, Gender gender)
    {
      return ordinal.ToString();
    }

    private static string GetDefaultLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return ordinal.ToString();
    }

    private static string GetPeriodShort(uint ordinal, Plural plural, Gender gender)
    {
      return ordinal.ToString() + ".";
    }


    // English
    private static string GetEnglishShort(uint ordinal, Plural plural, Gender gender)
    {
      string str;

      if ((ordinal % 10 == 1) && (ordinal % 100 != 11))
        str = "st";
      else if ((ordinal % 10 == 2) && (ordinal % 100 != 12))
        str = "nd";
      else if ((ordinal % 10 == 3) && (ordinal % 100 != 13))
        str = "rd";
      else
        str = "th";

      return ordinal.ToString() + str;
    }

    private static readonly string[] ENGLISH = 
    {
      "",
      "first",
      "second",
      "third",
      "fourth",
      "fifth",
      "sixth",
      "seventh",
      "eighth",
      "ninth",
      "tenth"
    };

    private static string GetEnglishLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return ENGLISH[(int)ordinal];
    }


    // German
    private static readonly string[] GERMAN =
    {
      "",
      "erstens",
      "zweitens",
      "drittens",
      "viertens",
      "fünftens",
      "sechstens",
      "siebtens",
      "achtens",
      "neuntens",
      "zehntens"
    };

    private static string GetGermanLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      if (ordinal == SmallOrdinal.First)
      {
        if (plural == Plural.Other)
          return "ersten";
        else if (gender == Gender.Neutral)
          return "erstes";
        else if (gender == Gender.Female)
          return "erste";
        else if (gender == Gender.Male)
          return "erster";
        else
          return GERMAN[(int)ordinal];
      }
      else if (ordinal == SmallOrdinal.Second)
      {
        if (plural == Plural.Other)
          return "zweiten";
        else if (gender == Gender.Neutral)
          return "zweites";
        else if (gender == Gender.Female)
          return "zweite";
        else if (gender == Gender.Male)
          return "zweiter";
        else
          return GERMAN[(int)ordinal];
      }
      else if (ordinal == SmallOrdinal.Third)
      {
        if (plural == Plural.Other)
          return "dritten";
        else if (gender == Gender.Neutral)
          return "drittes";
        else if (gender == Gender.Female)
          return "dritte";
        else if (gender == Gender.Male)
          return "dritter";
        else
          return GERMAN[(int)ordinal];
      }
      else
        return GERMAN[(int)ordinal];
    }


    // Dutch
    private static string GetDutchShort(uint ordinal, Plural plural, Gender gender)
    {
      return ordinal.ToString() + "e";
    }

    private static readonly string[] DUTCH =
    {
      "",
      "eerste",
      "tweede",
      "derde",
      "vierde",
      "vijfde",
      "zesde",
      "zevende",
      "achtste",
      "negende",
      "tiende"
    };

    private static string GetDutchLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return DUTCH[(int)ordinal];
    }


    // French
    private static string GetFrenchShort(uint ordinal, Plural plural, Gender gender)
    {
      string str;

      if (ordinal == 1)
      {
        if (gender == Gender.Male)
          str = "er";
        else
          str = "re";
      }
      else
        str = "e";

      return ordinal.ToString() + str;
    }

    private static readonly string[] FRENCH =
    {
      "",
      "premier",
      "deuxième",
      "troisième",
      "quatrième",
      "cinquième",
      "sixième",
      "septième",
      "huitième",
      "neuvième",
      "dixième"
    };

    private static string GetFrenchLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      if (ordinal == SmallOrdinal.First)
      {
        if (gender == Gender.Male)
          return "premier";
        else
          return "première";
      }
      else
        return FRENCH[(int)ordinal];
    }


    // Finnish
    private static readonly string[] FINNISH_SINGULARS =
    {
      "",
      "ensimmäinen",
      "toinen",
      "kolmas",
      "neljäs",
      "viides",
      "kuudes",
      "seitsemäs",
      "kahdeksas",
      "yhdeksäs",
      "kymmenes"
    };

    private static readonly string[] FINNISH_PLURALS =
    {
      "",
      "ensimmäiset",
      "toiset",
      "kolmannet",
      "neljännet",
      "viidennet",
      "kuudennet",
      "seitsennet",
      "kahdeksannet",
      "yhdeksännet",
      "kymmenennet"
    };

    private static string GetFinnishLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      if (plural == Plural.One)
        return FINNISH_SINGULARS[(int)ordinal];
      else
        return FINNISH_PLURALS[(int)ordinal];
    }


    // Estonian, TODO: plural forms
    private static readonly string[] ESTONIAN_PLURALS =
    {
      "",
      "esimene",
      "teine",
      "kolmas",
      "neljas",
      "viies",
      "kuues",
      "seitsmes",
      "kaheksas",
      "üheksas",
      "kümnes"
    };

    private static string GetEstonianLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return ESTONIAN_PLURALS[(int)ordinal];
    }


    // Danish
    private static readonly string[] DANISH =
    {
      "",
      "første",
      "anden",
      "tredje",
      "fjerde",
      "femte",
      "sjette",
      "syvende",
      "ottende",
      "niende",
      "tiende"
    };

    private static string GetDanishLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return DANISH[(int)ordinal];
    }


    // Swedish
    private static readonly string[] SWEDISH =
    {
      "",
      "första",
      "andra",
      "tredje",
      "fjärde",
      "femte",
      "sjätte",
      "sjunde",
      "åttonde",
      "nionde",
      "tionde"
    };

    private static string GetSwedishLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return SWEDISH[(int)ordinal];
    }


    // Norwegian, Bokmål
    private static readonly string[] NORWEGIAN_BOKMAL =
    {
      "",
      "første",
      "annen",
      "tredje",
      "fjerde",
      "femte",
      "sjette",
      "sjuende",
      "åttende",
      "niende",
      "tiende"
    };

    private static string GetNorwegianBokmalLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return NORWEGIAN_BOKMAL[(int)ordinal];
    }


    // Norwegian, Nynorsk
    private static readonly string[] NORWEGIAN_NYNORSK =
    {
      "",
      "første",
      "andre",
      "tredje",
      "fjerde",
      "femte",
      "sjette",
      "sjuande",
      "åttande",
      "niande",
      "tiande"
    };

    private static string GetNorwegianNynorskLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return NORWEGIAN_NYNORSK[(int)ordinal];
    }


    // Icelandic
    private static readonly string[] ICELANDIC =
    {
      "",
      "fyrsti",
      "annar",
      "þriðji",
      "fjórði",
      "fimmti",
      "sjötti",
      "sjöundi",
      "áttundi",
      "níundi",
      "tíundi"
    };

    private static string GetIcelandicLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return ICELANDIC[(int)ordinal];
    }

    
    // Japanese
    private static readonly string[] JAPANESE =
    {
      "",
      "一つ目",
      "二つ目",
      "三つ目",
      "四つ目",
      "五つ目",
      "六つ目",
      "七つ目",
      "八つ目",
      "九つ目",
      "十"
    };

    private static string GetJapaneseLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return JAPANESE[(int)ordinal];
    }

    private static string GetJapaneseShort(uint ordinal, Plural plural, Gender gender)
    {
      if (IsShortOrdinal(ordinal))
        return GetJapaneseLong((SmallOrdinal)ordinal, plural, gender);
      else
        return ordinal.ToString() + "目";
    }
    

    // Korean
    private static readonly string[] KOREAN =
    {
      "",
      "첫째",
      "둘째",
      "셋째",
      "넷째",
      "다섯째",
      "여섯째",
      "일곱째",
      "여덟째",
      "아홉째",
      "열째"
    };

    private static string GetKoreanLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return KOREAN[(int)ordinal];
    }

    private static string GetKoreanShort(uint ordinal, Plural plural, Gender gender)
    {
      if (IsShortOrdinal(ordinal))
        return GetKoreanLong((SmallOrdinal)ordinal, plural, gender);
      else
        return ordinal.ToString() + "째";
    }
    

    // Simplified Chinese
    private static readonly string[] SIMPLIFIED_CHINESE = 
    {
      "",
      "第一",
      "第二",
      "第三",
      "第四",
      "第五",
      "第六",
      "第七",
      "第八",
      "第九",
      "第十"
    };

    private static string GetSimplifiedChineseLong(SmallOrdinal ordinal, Plural plural, Gender gender)
    {
      return SIMPLIFIED_CHINESE[(int)ordinal];
    }

    private static string GetSimplifiedChineseShort(uint ordinal, Plural plural, Gender gender)
    {
      if (IsShortOrdinal(ordinal))
        return GetSimplifiedChineseLong((SmallOrdinal)ordinal, plural, gender);
      else
        return "第" + ordinal.ToString();
    }


    static Ordinal()
    {
      Register("", GetDefaultLong, GetDefaultShort);

      Register("en", GetEnglishLong, GetEnglishShort);
      Register("de", GetGermanLong, GetPeriodShort);
      Register("nl", GetDutchLong, GetDutchShort);
      Register("fr", GetFrenchLong, GetFrenchShort);

      Register("fi", GetFinnishLong, GetPeriodShort);
      Register("et", GetEstonianLong, GetPeriodShort);
      Register("sv", GetSwedishLong, GetPeriodShort);
      Register("da", GetDanishLong,  GetPeriodShort);
      Register("no", GetNorwegianBokmalLong, GetPeriodShort);
      Register("nb", GetNorwegianBokmalLong, GetPeriodShort);
      Register("nn", GetNorwegianNynorskLong, GetPeriodShort);
      Register("is", GetIcelandicLong, GetPeriodShort);

      Register("ja", GetJapaneseLong, GetJapaneseShort);
      Register("ko", GetKoreanLong, GetKoreanShort);

      Register("zh", GetSimplifiedChineseLong, GetSimplifiedChineseShort);
      Register("zh-Hans", GetSimplifiedChineseLong, GetSimplifiedChineseShort);
    }
  }
}
