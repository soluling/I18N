import { Language, PluralForm, Gender } from "./language";

/**
 * Enumeration that specifies the string form of an ordinal.
 */
export enum OrdinalStringForm
{
  Long,  // Long string form such as "first" and "fourth"
  Short  // Short string form such as "1st" and "4th"
}

/**
 * Enumeration that specifies what plural group a language belongs to.
 * The algorithm is named based on the main language of the group.
 * For example PluralGroup.Japanese is not only applied to Japanese but all the other languages where plural rules are the same as in Japanese (expecially Asian ones).
 */
export enum SmallOrdinal
{
  First = 1, // first or 1st and localized version
  Second,    // second or 2nd
  Third,     // third or 3rd
  Fourth,    // fourth or 4th
  Fifth,     // fifth or 5th
  Sixth,     // sixth or 6th
  Seventh,   // seventh or 7th
  Eighth,    // eighth or 8th
  Ninth,     // ninth or 9th
  Tenth      // tenth or 10th
};

interface OrdinalLongProc
{
  (ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string;
}

interface OrdinalShortProc
{
  (ordinal: number, plural: PluralForm, gender: Gender): string;
}

class OrdinalData
{
  LongProc: OrdinalLongProc;
  ShortProc: OrdinalShortProc;
}

function initialize(target: any) 
{
  target.initialize();
}

/**
 * Implements a formatting a number into its ordinal form.
 * 
 * For example in English "This is my 1st car" and "This is my 2nd card". Note the ending after the number.
 * It is either st, nd, rd, or th depending on the number.
 * Some other countries use different endings and the rules vary. Some countries just add pediod in the end
 * like in Finnish "Tämä on minun 1. autoni" or "Tämä on minun 2. autoni".
 */
@initialize
export class Ordinal extends Language
{
  private static datas: { [id: string]: OrdinalData; } = { };

  /**
   * Converts an ordinal number to a string such as "1st" or "first".
   * @param form String form to be used.
   * @param ordinal Ordinal number.
   * @param plural Specifies the plural form.
   * @param gender Specifies the gender.
   * @return {string} The formatted string.
   */
  public static Format(
    form: OrdinalStringForm,
    ordinal: number, 
    plural: PluralForm = PluralForm.One,
    gender: Gender = Gender.Neutral): string 
  {
    if (form == OrdinalStringForm.Short)
      return Ordinal.FormatShort(ordinal, plural, gender);
    else
      return Ordinal.FormatLong(ordinal, plural, gender);
  }

  /**
   * Converts an ordinal number to a string using the short format such as "1st" and "4th".
   * @param ordinal Ordinal number.
   * @param plural Specifies the plural form.
   * @param gender Specifies the gender.
   * @return {string} The formatted string.
   */
  public static FormatShort(
    ordinal: number, 
    plural: PluralForm = PluralForm.One,
    gender: Gender =  Gender.Neutral): string
  {
    return this.GetData().ShortProc(ordinal, plural, gender);
  }

  /**
   * Converts an ordinal number to a string using the long format such as "first" and "fourth".
   * Only the first 10 ordinals can be formatted to the long format. The rest of the ordinals will always
   * be formatted using the short format such as "21st" and "24th".
   * @param ordinal Ordinal number.
   * @param plural Specifies the plural form.
   * @param gender Specifies the gender.
   * @return {string} The formatted string.
   */
  public static FormatLong(
    ordinal: number, 
    plural: PluralForm = PluralForm.One,
    gender: Gender =  Gender.Neutral): string
  {
    if (this.IsShortOrdinal(ordinal))
      return this.GetData().LongProc(ordinal, plural, gender);
    else
      return this.FormatShort(ordinal, plural, gender);
  }

  /**
   * Register formatting functions for a language.
   * @param id ISO 639-1 language code of the language.
   * @param longProc Function that converts a small ordianal to a long string.
   * @param shortProc Function that converts an ordianal to a short string. 
   */
  public static Register(id: string, longProc: OrdinalLongProc, shortProc: OrdinalShortProc): void
  {
    var data: OrdinalData = this.datas[id];

    if (data == null)
    {
      data = new OrdinalData();
      this.datas[id] = data;
    }

    data.LongProc = longProc;
    data.ShortProc = shortProc;
  }


  private static GetData(): OrdinalData
  {
    var id = this.locale;

    if (this.datas[id] == null)
      id = "";

    return this.datas[id];
  }

  private static IsShortOrdinal(value: number): boolean
  {
    return (value >= SmallOrdinal.First) && (value <= SmallOrdinal.Tenth);
  }


  private static GetDefaultShort(ordinal: number, plural: PluralForm, gender: Gender): string
  {
    return ordinal.toString();
  }

  private static GetDefaultLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    return ordinal.toString();
  }

  private static GetPeriodShort(ordinal: number, plural: PluralForm, gender: Gender): string
  {
    return ordinal.toString() + ".";
  }


  // English
  private static GetEnglishShort(ordinal: number, plural: PluralForm, gender: Gender): string
  {
    var str: string;

    if ((ordinal % 10 == 1) && (ordinal % 100 != 11))
      str = "st";
    else if ((ordinal % 10 == 2) && (ordinal % 100 != 12))
      str = "nd";
    else if ((ordinal % 10 == 3) && (ordinal % 100 != 13))
      str = "rd";
    else
      str = "th";

    return ordinal.toString() + str;
  }

  private static GetEnglishLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const ENGLISH: string[] =
    [
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
    ];

    return ENGLISH[ordinal];
  }


  // German
  private static GetGermanLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const GERMAN: string[] =
    [
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
    ];

    if (ordinal == SmallOrdinal.First)
    {
      if (plural == PluralForm.Other)
        return "ersten";
      else if (gender == Gender.Neutral)
        return "erstes";
      else if (gender == Gender.Female)
        return "erste";
      else if (gender == Gender.Male)
        return "erster";
      else
        return GERMAN[ordinal];
    }
    else if (ordinal == SmallOrdinal.Second)
    {
      if (plural == PluralForm.Other)
        return "zweiten";
      else if (gender == Gender.Neutral)
        return "zweites";
      else if (gender == Gender.Female)
        return "zweite";
      else if (gender == Gender.Male)
        return "zweiter";
      else
        return GERMAN[ordinal];
    }
    else if (ordinal == SmallOrdinal.Third)
    {
      if (plural == PluralForm.Other)
        return "dritten";
      else if (gender == Gender.Neutral)
        return "drittes";
      else if (gender == Gender.Female)
        return "dritte";
      else if (gender == Gender.Male)
        return "dritter";
      else
        return GERMAN[ordinal];
    }
    else
      return GERMAN[ordinal];
  }


  // Dutch
  private static GetDutchShort(ordinal: number, plural: PluralForm, gender: Gender): string
  {
    return ordinal.toString() + "e";
  }

  private static GetDutchLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const DUTCH: string[] =
    [
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
    ];

    return DUTCH[ordinal];
  }


  // French
  private static GetFrenchShort(ordinal: number, plural: PluralForm, gender: Gender): string
  {
    var str: string;

    if (ordinal == 1)
    {
      if (gender == Gender.Male)
        str = "er";
      else
        str = "re";
    }
    else
      str = "e";

    return ordinal.toString() + str;
  }

  private static GetFrenchLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const FRENCH: string[] =
    [
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
    ];

    if (ordinal == SmallOrdinal.First)
    {
      if (gender == Gender.Male)
        return "premier";
      else
        return "première";
    }
    else
      return FRENCH[ordinal];
  }


  // Finnish
  private static GetFinnishLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const FINNISH_SINGULARS: string[] =
    [
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
    ];

    const FINNISH_PLURALS: string[] =
    [
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
    ];

    if (plural == PluralForm.One)
      return FINNISH_SINGULARS[ordinal];
    else
      return FINNISH_PLURALS[ordinal];
  }


  // Estonian, TODO: plural forms
  private static GetEstonianLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const ESTONIAN_PLURALS: string[] =
    [
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
    ];

    return ESTONIAN_PLURALS[ordinal];
  }


  // Danish
  private static GetDanishLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const DANISH: string[] =
    [
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
    ];

    return DANISH[ordinal];
  }


  // Swedish
  private static GetSwedishLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const SWEDISH: string[] =
    [
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
    ];

    return SWEDISH[ordinal];
  }


  // Norwegian, Bokmål
  private static GetNorwegianBokmalLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const NORWEGIAN_BOKMAL: string[] =
    [
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
    ];

    return NORWEGIAN_BOKMAL[ordinal];
  }


  // Norwegian, Nynorsk
  private static GetNorwegianNynorskLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const NORWEGIAN_NYNORSK: string[] =
    [
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
    ];

    return NORWEGIAN_NYNORSK[ordinal];
  }


  // Icelandic
  private static GetIcelandicLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const ICELANDIC: string[] =
    [
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
    ];

    return ICELANDIC[ordinal];
  }

  
  // Japanese
  private static GetJapaneseLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const JAPANESE: string[] =
    [
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
    ];

    return JAPANESE[ordinal];
  }

  private static GetJapaneseShort(ordinal: number, plural: PluralForm, gender: Gender): string
  {
    if (this.IsShortOrdinal(ordinal))
      return this.GetJapaneseLong(ordinal, plural, gender);
    else
      return ordinal.toString() + "目";
  }
  

  // Korean
  private static GetKoreanLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const KOREAN: string[] =
    [
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
    ];

    return KOREAN[ordinal];
  }

  private static GetKoreanShort(ordinal: number, plural: PluralForm, gender: Gender): string
  {
    if (this.IsShortOrdinal(ordinal))
      return this.GetKoreanLong(ordinal, plural, gender);
    else
      return ordinal.toString() + "째";
  }
  

  // Simplified Chinese
  private static GetSimplifiedChineseLong(ordinal: SmallOrdinal, plural: PluralForm, gender: Gender): string
  {
    const SIMPLIFIED_CHINESE: string[] =
    [
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
    ];

    return SIMPLIFIED_CHINESE[ordinal];
  }

  private static GetSimplifiedChineseShort(ordinal: number, plural: PluralForm, gender: Gender): string
  {
    if (this.IsShortOrdinal(ordinal))
      return this.GetSimplifiedChineseLong(ordinal, plural, gender);
    else
      return "第" + ordinal.toString();
  }


  // Static initializer that is called automatically. Register ordinal functions.
  static initialize()
  {
    if (this.datas[""] != null)
      return;

    this.Register("", this.GetDefaultLong, this.GetDefaultShort);

    this.Register("en", this.GetEnglishLong, this.GetEnglishShort);
    this.Register("de", this.GetGermanLong, this.GetPeriodShort);
    this.Register("nl", this.GetDutchLong, this.GetDutchShort);
    this.Register("fr", this.GetFrenchLong, this.GetFrenchShort);

    this.Register("fi", this.GetFinnishLong, this.GetPeriodShort);
    this.Register("et", this.GetEstonianLong, this.GetPeriodShort);
    this.Register("sv", this.GetSwedishLong, this.GetPeriodShort);
    this.Register("da", this.GetDanishLong, this.GetPeriodShort);
    this.Register("no", this.GetNorwegianBokmalLong, this.GetPeriodShort);
    this.Register("nb", this.GetNorwegianBokmalLong, this.GetPeriodShort);
    this.Register("nn", this.GetNorwegianNynorskLong, this.GetPeriodShort);
    this.Register("is", this.GetIcelandicLong, this.GetPeriodShort);

    this.Register("ja", this.GetJapaneseShort, this.GetJapaneseLong);
    this.Register("ko", this.GetKoreanShort,   this.GetKoreanLong);

    this.Register("zh", this.GetSimplifiedChineseLong, this.GetSimplifiedChineseShort);
    this.Register("zh-Hans", this.GetSimplifiedChineseLong, this.GetSimplifiedChineseShort);
  }
}