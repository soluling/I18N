// Generated from CLDR data. Do not edit.
namespace Soluling
{
  /// <summary>
  /// Contains grammatical number rules for each language.
  /// </summary>
  public class PluralData
  {
    /// <summary>
    /// Get a plural form for Bamanankan, Tibetan, Dzongkha, Indonesian, Igbo, Yi, in, Japanese, jbo, Javanese, jw, Makonde, Kabuverdianu, Khmer, Korean, Lakota, Lao, Malay, Burmese, N'ko, root, Sakha, Koyraboro Senni, Sango, Thai, Tongan, Vietnamese, Wolof, Yoruba, yue, Chinese, Simplified.
    /// </summary>
    /// <param name="n">Absolute value of the source number (integer and decimals) (e.g. 9.870 => 9.87).</param>
    /// <param name="i">Integer digits of n (e.g. 9.870 => 9).</param>
    /// <param name="v">Number of visible fraction digits in n, with trailing zeros (e.g. 9.870 => 3).</param>
    /// <param name="w">Number of visible fraction digits in n, without trailing zeros (e.g. 9.870 => 2).</param>
    /// <param name="f">Visible fractional digits in n, with trailing zeros (e.g. 9.870 => 870).</param>
    /// <param name="t">Visible fractional digits in n, without trailing zeros (e.g. 9.870 => 87).</param>
    /// <returns>Plural form.</returns>
    public static Plural GetSinglePlural(decimal n, int i, int v, int w, int f, int t)
    {
      // other: @integer 0~15, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      return Plural.Other;
    }

    // Amharic, Assamese, Bangla, Persian, Gujarati, Hindi, Kannada, Marathi, isiZulu
    private static Plural GetAmPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: i = 0 or n = 1 @integer 0, 1 @decimal 0.0~1.0, 0.00~0.04
      // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 1.1~2.6, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((i == 0) || (n == 1))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Fulah, French, Armenian, Kabyle
    private static Plural GetFfPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: i = 0,1 @integer 0, 1 @decimal 0.0~1.5
      // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 2.0~3.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if (((i == 0) || (i == 1)))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Asturian, Catalan, German, English, Estonian, Finnish, Western Frisian, Galician, Italian, ji, Dutch, Swedish, Kiswahili, Urdu, Yiddish
    private static Plural GetAstPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: i = 1 and v = 0 @integer 1
      // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((i == 1) && (v == 0))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Sinhala
    private static Plural GetSinhalaPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 0,1 or i = 0 and f = 1 @integer 0, 1 @decimal 0.0, 0.1, 1.0, 0.00, 0.01, 1.00, 0.000, 0.001, 1.000, 0.0000, 0.0001, 1.0000
      // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.2~0.9, 1.1~1.8, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if (((n == 0) || (n == 1)) || (i == 0) && (f == 1))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Akan, bh, guw, Lingala, Malagasy, Sesotho sa Leboa, Punjabi, Tigrinya, wa
    private static Plural GetAkPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 0..1 @integer 0, 1 @decimal 0.0, 1.0, 0.00, 1.00, 0.000, 1.000, 0.0000, 1.0000
      // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n >= 0) && (n <= 1))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Central Atlas Tamazight
    private static Plural GetCentralAtlasTamazightPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 0..1 or n = 11..99 @integer 0, 1, 11~24 @decimal 0.0, 1.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24.0
      // other: @integer 2~10, 100~106, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n >= 0) && (n <= 1) || (n >= 11) && (n <= 99))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Portuguese
    private static Plural GetPortuguesePlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 0..2 and n != 2 @integer 0, 1 @decimal 0.0, 1.0, 0.00, 1.00, 0.000, 1.000, 0.0000, 1.0000
      // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n >= 0) && (n <= 1))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Afrikaans, Asu, Azerbaijani, Latin, Bemba, Bena, Bulgarian, Bodo, Chechen, Chiga, Cherokee, ckb, Divehi, Ewe, Greek, Esperanto, Spanish, Basque, Faroese, Friulian, Swiss German, Hausa, Hawaiian, Hungarian, Ngomba, Machame, Georgian, kaj, kcg, Kazakh, Kako, Greenlandic, Kashmiri, Shambala, Central Kurdish, Kyrgyz, Luxembourgish, Ganda, Masai, Metaʼ, Malayalam, Mongolian, Cyrillic, nah, Norwegian Bokmål, North Ndebele, Nepali, Norwegian Nynorsk, Ngiemboon, Norwegian, South Ndebele, ny, Nyankole, Oromo, Odia, Ossetic, Papiamento, Pashto, Romansh, Rombo, Rwa, Samburu, sdh, Sena, Shona, Somali, Albanian, siSwati, Saho, Sesotho, Syriac, Tamil, Telugu, Teso, Tigre, Turkmen, Setswana, Turkish, Tsonga, Uyghur, Uzbek, Latin, Venda, Volapük, Vunjo, Walser, isiXhosa, Soga
    private static Plural GetAfPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
      // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.9, 1.1~1.6, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 1))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Portuguese (Portugal)
    private static Plural GetPortuguesePortugalPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 1 and v = 0 @integer 1
      // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 1) && (v == 0))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Danish
    private static Plural GetDanishPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 1 or t != 0 and i = 0,1 @integer 1 @decimal 0.1~1.6
      // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 2.0~3.4, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 1) || (t != 0) && ((i == 0) || (i == 1)))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Icelandic
    private static Plural GetIcelandicPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: t = 0 and i % 10 = 1 and i % 100 != 11 or t != 0 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 0.1~1.6, 10.1, 100.1, 1000.1, …
      // other: @integer 0, 2~16, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((t == 0) && (i % 10 == 1) && (i % 100 != 11) || (t != 0))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Macedonian
    private static Plural GetMacedonianPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: v = 0 and i % 10 = 1 or f % 10 = 1 @integer 1, 11, 21, 31, 41, 51, 61, 71, 101, 1001, … @decimal 0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 10.1, 100.1, 1000.1, …
      // other: @integer 0, 2~10, 12~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 0.2~1.0, 1.2~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((v == 0) && (i % 10 == 1) || (f % 10 == 1))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Filipino, tl
    private static Plural GetFilipinoPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: v = 0 and i = 1,2,3 or v = 0 and i % 10 != 4,6,9 or v != 0 and f % 10 != 4,6,9 @integer 0~3, 5, 7, 8, 10~13, 15, 17, 18, 20, 21, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.3, 0.5, 0.7, 0.8, 1.0~1.3, 1.5, 1.7, 1.8, 2.0, 2.1, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      // other: @integer 4, 6, 9, 14, 16, 19, 24, 26, 104, 1004, … @decimal 0.4, 0.6, 0.9, 1.4, 1.6, 1.9, 2.4, 2.6, 10.4, 100.4, 1000.4, …
      if ((v == 0) && ((i == 1) || (i == 2) || (i == 3)) || (v == 0) && ((i % 10 != 4) || (i % 10 != 6) || (i % 10 != 9)) || (v != 0) && ((f % 10 != 4) || (f % 10 != 6) || (f % 10 != 9)))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Latvian, Prussian
    private static Plural GetLatvianPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // zero: n % 10 = 0 or n % 100 = 11..19 or v = 2 and f % 100 = 11..19 @integer 0, 10~20, 30, 40, 50, 60, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      // one: n % 10 = 1 and n % 100 != 11 or v = 2 and f % 10 = 1 and f % 100 != 11 or v != 2 and f % 10 = 1 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 0.1, 1.0, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 10.1, 100.1, 1000.1, …
      // other: @integer 2~9, 22~29, 102, 1002, … @decimal 0.2~0.9, 1.2~1.9, 10.2, 100.2, 1000.2, …
      if ((n % 10 == 0) || (n % 100 >= 11) && (n % 100 <= 19) || (v == 2) && (f % 100 >= 11) && (f % 100 <= 19))
        return Plural.Zero;
      else if ((n % 10 == 1) && (n % 100 != 11) || (v == 2) && (f % 10 == 1) && (f % 100 != 11) || (v != 2) && (f % 10 == 1))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Langi
    private static Plural GetLangiPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // zero: n = 0 @integer 0 @decimal 0.0, 0.00, 0.000, 0.0000
      // one: i = 0,1 and n != 0 @integer 1 @decimal 0.1~1.6
      // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 2.0~3.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 0))
        return Plural.Zero;
      else if (((i == 0) || (i == 1)) && (n != 0))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Colognian
    private static Plural GetColognianPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // zero: n = 0 @integer 0 @decimal 0.0, 0.00, 0.000, 0.0000
      // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
      // other: @integer 2~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 0))
        return Plural.Zero;
      else if ((n == 1))
        return Plural.One;
      else
        return Plural.Other;
    }

    // Inuktitut, Latin, Cornish, Nama, Northern Sami, Sami, Southern, smi, Sami, Lule, Sami, Inari, Sami, Skolt
    private static Plural GetIuPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
      // two: n = 2 @integer 2 @decimal 2.0, 2.00, 2.000, 2.0000
      // other: @integer 0, 3~17, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.9, 1.1~1.6, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 1))
        return Plural.One;
      else if ((n == 2))
        return Plural.Two;
      else
        return Plural.Other;
    }

    // Tachelhit
    private static Plural GetTachelhitPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: i = 0 or n = 1 @integer 0, 1 @decimal 0.0~1.0, 0.00~0.04
      // few: n = 2..10 @integer 2~10 @decimal 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 2.00, 3.00, 4.00, 5.00, 6.00, 7.00, 8.00
      // other: @integer 11~26, 100, 1000, 10000, 100000, 1000000, … @decimal 1.1~1.9, 2.1~2.7, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((i == 0) || (n == 1))
        return Plural.One;
      else if ((n >= 2) && (n <= 10))
        return Plural.Few;
      else
        return Plural.Other;
    }

    // mo, Romanian
    private static Plural GetRomanianPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: i = 1 and v = 0 @integer 1
      // few: v != 0 or n = 0 or n != 1 and n % 100 = 1..19 @integer 0, 2~16, 101, 1001, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      // other: @integer 20~35, 100, 1000, 10000, 100000, 1000000, …
      if ((i == 1) && (v == 0))
        return Plural.One;
      else if ((v != 0) || (n == 0) || (n != 1) && (n % 100 >= 1) && (n % 100 <= 19))
        return Plural.Few;
      else
        return Plural.Other;
    }

    // Bosnian, Latin, Croatian, sh, Serbian, Cyrillic
    private static Plural GetBsPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: v = 0 and i % 10 = 1 and i % 100 != 11 or f % 10 = 1 and f % 100 != 11 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 10.1, 100.1, 1000.1, …
      // few: v = 0 and i % 10 = 2..4 and i % 100 != 12..14 or f % 10 = 2..4 and f % 100 != 12..14 @integer 2~4, 22~24, 32~34, 42~44, 52~54, 62, 102, 1002, … @decimal 0.2~0.4, 1.2~1.4, 2.2~2.4, 3.2~3.4, 4.2~4.4, 5.2, 10.2, 100.2, 1000.2, …
      // other: @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 0.5~1.0, 1.5~2.0, 2.5~2.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((v == 0) && (i % 10 == 1) && (i % 100 != 11) || (f % 10 == 1) && (f % 100 != 11))
        return Plural.One;
      else if ((v == 0) && (i % 10 >= 2) && (i % 10 <= 4) && !((i % 100 >= 12) && (i % 100 <= 14)) || (f % 10 >= 2) && (f % 10 <= 4) && !((f % 100 >= 12) && (f % 100 <= 14)))
        return Plural.Few;
      else
        return Plural.Other;
    }

    // Scottish Gaelic
    private static Plural GetScottishGaelicPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 1,11 @integer 1, 11 @decimal 1.0, 11.0, 1.00, 11.00, 1.000, 11.000, 1.0000
      // two: n = 2,12 @integer 2, 12 @decimal 2.0, 12.0, 2.00, 12.00, 2.000, 12.000, 2.0000
      // few: n = 3..10,13..19 @integer 3~10, 13~19 @decimal 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 3.00
      // other: @integer 0, 20~34, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.9, 1.1~1.6, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if (((n == 1) || (n == 11)))
        return Plural.One;
      else if (((n == 2) || (n == 12)))
        return Plural.Two;
      else if (((n >= 3) && (n >= 13) && (n <= 19)))
        return Plural.Few;
      else
        return Plural.Other;
    }

    // Slovenian
    private static Plural GetSlovenianPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: v = 0 and i % 100 = 1 @integer 1, 101, 201, 301, 401, 501, 601, 701, 1001, …
      // two: v = 0 and i % 100 = 2 @integer 2, 102, 202, 302, 402, 502, 602, 702, 1002, …
      // few: v = 0 and i % 100 = 3..4 or v != 0 @integer 3, 4, 103, 104, 203, 204, 303, 304, 403, 404, 503, 504, 603, 604, 703, 704, 1003, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      // other: @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, …
      if ((v == 0) && (i % 100 == 1))
        return Plural.One;
      else if ((v == 0) && (i % 100 == 2))
        return Plural.Two;
      else if ((v == 0) && (i % 100 >= 3) && (i % 100 <= 4) || (v != 0))
        return Plural.Few;
      else
        return Plural.Other;
    }

    // Lower Sorbian, Upper Sorbian
    private static Plural GetLowerSorbianPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: v = 0 and i % 100 = 1 or f % 100 = 1 @integer 1, 101, 201, 301, 401, 501, 601, 701, 1001, … @decimal 0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 10.1, 100.1, 1000.1, …
      // two: v = 0 and i % 100 = 2 or f % 100 = 2 @integer 2, 102, 202, 302, 402, 502, 602, 702, 1002, … @decimal 0.2, 1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2, 10.2, 100.2, 1000.2, …
      // few: v = 0 and i % 100 = 3..4 or f % 100 = 3..4 @integer 3, 4, 103, 104, 203, 204, 303, 304, 403, 404, 503, 504, 603, 604, 703, 704, 1003, … @decimal 0.3, 0.4, 1.3, 1.4, 2.3, 2.4, 3.3, 3.4, 4.3, 4.4, 5.3, 5.4, 6.3, 6.4, 7.3, 7.4, 10.3, 100.3, 1000.3, …
      // other: @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 0.5~1.0, 1.5~2.0, 2.5~2.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((v == 0) && (i % 100 == 1) || (f % 100 == 1))
        return Plural.One;
      else if ((v == 0) && (i % 100 == 2) || (f % 100 == 2))
        return Plural.Two;
      else if ((v == 0) && (i % 100 >= 3) && (i % 100 <= 4) || (f % 100 >= 3) && (f % 100 <= 4))
        return Plural.Few;
      else
        return Plural.Other;
    }

    // Hebrew, iw
    private static Plural GetHebrewPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: i = 1 and v = 0 @integer 1
      // two: i = 2 and v = 0 @integer 2
      // many: v = 0 and n != 0..10 and n % 10 = 0 @integer 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000, 10000, 100000, 1000000, …
      // other: @integer 0, 3~17, 101, 1001, … @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((i == 1) && (v == 0))
        return Plural.One;
      else if ((i == 2) && (v == 0))
        return Plural.Two;
      else if ((v == 0) && !((n >= 0) && (n <= 10)) && (n % 10 == 0))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Czech, Slovak
    private static Plural GetCzechPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: i = 1 and v = 0 @integer 1
      // few: i = 2..4 and v = 0 @integer 2~4
      // many: v != 0   @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      // other: @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, …
      if ((i == 1) && (v == 0))
        return Plural.One;
      else if ((i >= 2) && (i <= 4) && (v == 0))
        return Plural.Few;
      else if ((v != 0))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Polish
    private static Plural GetPolishPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: i = 1 and v = 0 @integer 1
      // few: v = 0 and i % 10 = 2..4 and i % 100 != 12..14 @integer 2~4, 22~24, 32~34, 42~44, 52~54, 62, 102, 1002, …
      // many: v = 0 and i != 1 and i % 10 = 0..1 or v = 0 and i % 10 = 5..9 or v = 0 and i % 100 = 12..14 @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, …
      // other: @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((i == 1) && (v == 0))
        return Plural.One;
      else if ((v == 0) && (i % 10 >= 2) && (i % 10 <= 4) && !((i % 100 >= 12) && (i % 100 <= 14)))
        return Plural.Few;
      else if ((v == 0) && (i != 1) && (i % 10 >= 0) && (i % 10 <= 1) || (v == 0) && (i % 10 >= 5) && (i % 10 <= 9) || (v == 0) && (i % 100 >= 12) && (i % 100 <= 14))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Belarusian
    private static Plural GetBelarusianPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n % 10 = 1 and n % 100 != 11 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 1.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 101.0, 1001.0, …
      // few: n % 10 = 2..4 and n % 100 != 12..14 @integer 2~4, 22~24, 32~34, 42~44, 52~54, 62, 102, 1002, … @decimal 2.0, 3.0, 4.0, 22.0, 23.0, 24.0, 32.0, 33.0, 102.0, 1002.0, …
      // many: n % 10 = 0 or n % 10 = 5..9 or n % 100 = 11..14 @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      // other: @decimal 0.1~0.9, 1.1~1.7, 10.1, 100.1, 1000.1, …
      if ((n % 10 == 1) && (n % 100 != 11))
        return Plural.One;
      else if ((n % 10 >= 2) && (n % 10 <= 4) && !((n % 100 >= 12) && (n % 100 <= 14)))
        return Plural.Few;
      else if ((n % 10 == 0) || (n % 10 >= 5) && (n % 10 <= 9) || (n % 100 >= 11) && (n % 100 <= 14))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Lithuanian
    private static Plural GetLithuanianPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n % 10 = 1 and n % 100 != 11..19 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, … @decimal 1.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 101.0, 1001.0, …
      // few: n % 10 = 2..9 and n % 100 != 11..19 @integer 2~9, 22~29, 102, 1002, … @decimal 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 22.0, 102.0, 1002.0, …
      // many: f != 0   @decimal 0.1~0.9, 1.1~1.7, 10.1, 100.1, 1000.1, …
      // other: @integer 0, 10~20, 30, 40, 50, 60, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n % 10 == 1) && !((n % 100 >= 11) && (n % 100 <= 19)))
        return Plural.One;
      else if ((n % 10 >= 2) && (n % 10 <= 9) && !((n % 100 >= 11) && (n % 100 <= 19)))
        return Plural.Few;
      else if ((f != 0))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Maltese
    private static Plural GetMaltesePlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
      // few: n = 0 or n % 100 = 2..10 @integer 0, 2~10, 102~107, 1002, … @decimal 0.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 10.0, 102.0, 1002.0, …
      // many: n % 100 = 11..19 @integer 11~19, 111~117, 1011, … @decimal 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 111.0, 1011.0, …
      // other: @integer 20~35, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 1))
        return Plural.One;
      else if ((n == 0) || (n % 100 >= 2) && (n % 100 <= 10))
        return Plural.Few;
      else if ((n % 100 >= 11) && (n % 100 <= 19))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Russian, Ukrainian
    private static Plural GetRussianPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: v = 0 and i % 10 = 1 and i % 100 != 11 @integer 1, 21, 31, 41, 51, 61, 71, 81, 101, 1001, …
      // few: v = 0 and i % 10 = 2..4 and i % 100 != 12..14 @integer 2~4, 22~24, 32~34, 42~44, 52~54, 62, 102, 1002, …
      // many: v = 0 and i % 10 = 0 or v = 0 and i % 10 = 5..9 or v = 0 and i % 100 = 11..14 @integer 0, 5~19, 100, 1000, 10000, 100000, 1000000, …
      // other: @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((v == 0) && (i % 10 == 1) && (i % 100 != 11))
        return Plural.One;
      else if ((v == 0) && (i % 10 >= 2) && (i % 10 <= 4) && !((i % 100 >= 12) && (i % 100 <= 14)))
        return Plural.Few;
      else if ((v == 0) && (i % 10 == 0) || (v == 0) && (i % 10 >= 5) && (i % 10 <= 9) || (v == 0) && (i % 100 >= 11) && (i % 100 <= 14))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Breton
    private static Plural GetBretonPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n % 10 = 1 and n % 100 != 11,71,91 @integer 1, 21, 31, 41, 51, 61, 81, 101, 1001, … @decimal 1.0, 21.0, 31.0, 41.0, 51.0, 61.0, 81.0, 101.0, 1001.0, …
      // two: n % 10 = 2 and n % 100 != 12,72,92 @integer 2, 22, 32, 42, 52, 62, 82, 102, 1002, … @decimal 2.0, 22.0, 32.0, 42.0, 52.0, 62.0, 82.0, 102.0, 1002.0, …
      // few: n % 10 = 3..4,9 and n % 100 != 10..19,70..79,90..99 @integer 3, 4, 9, 23, 24, 29, 33, 34, 39, 43, 44, 49, 103, 1003, … @decimal 3.0, 4.0, 9.0, 23.0, 24.0, 29.0, 33.0, 34.0, 103.0, 1003.0, …
      // many: n != 0 and n % 1000000 = 0 @integer 1000000, … @decimal 1000000.0, 1000000.00, 1000000.000, …
      // other: @integer 0, 5~8, 10~20, 100, 1000, 10000, 100000, … @decimal 0.0~0.9, 1.1~1.6, 10.0, 100.0, 1000.0, 10000.0, 100000.0, …
      if ((n % 10 == 1) && ((n % 100 != 11) || (n % 100 != 71) || (n % 100 != 91)))
        return Plural.One;
      else if ((n % 10 == 2) && ((n % 100 != 12) || (n % 100 != 72) || (n % 100 != 92)))
        return Plural.Two;
      else if (((n % 10 >= 3) && (n % 10 <= 4) || (n % 10 == 9)) && ((n % 100 >= 10) && (n % 100 >= 70) && (n % 100 >= 90) && (n % 100 <= 99)))
        return Plural.Few;
      else if ((n != 0) && (n % 1000000 == 0))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Irish
    private static Plural GetIrishPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
      // two: n = 2 @integer 2 @decimal 2.0, 2.00, 2.000, 2.0000
      // few: n = 3..6 @integer 3~6 @decimal 3.0, 4.0, 5.0, 6.0, 3.00, 4.00, 5.00, 6.00, 3.000, 4.000, 5.000, 6.000, 3.0000, 4.0000, 5.0000, 6.0000
      // many: n = 7..10 @integer 7~10 @decimal 7.0, 8.0, 9.0, 10.0, 7.00, 8.00, 9.00, 10.00, 7.000, 8.000, 9.000, 10.000, 7.0000, 8.0000, 9.0000, 10.0000
      // other: @integer 0, 11~25, 100, 1000, 10000, 100000, 1000000, … @decimal 0.0~0.9, 1.1~1.6, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 1))
        return Plural.One;
      else if ((n == 2))
        return Plural.Two;
      else if ((n >= 3) && (n <= 6))
        return Plural.Few;
      else if ((n >= 7) && (n <= 10))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Manx
    private static Plural GetManxPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // one: v = 0 and i % 10 = 1 @integer 1, 11, 21, 31, 41, 51, 61, 71, 101, 1001, …
      // two: v = 0 and i % 10 = 2 @integer 2, 12, 22, 32, 42, 52, 62, 72, 102, 1002, …
      // few: v = 0 and i % 100 = 0,20,40,60,80 @integer 0, 20, 40, 60, 80, 100, 120, 140, 1000, 10000, 100000, 1000000, …
      // many: v != 0   @decimal 0.0~1.5, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      // other: @integer 3~10, 13~19, 23, 103, 1003, …
      if ((v == 0) && (i % 10 == 1))
        return Plural.One;
      else if ((v == 0) && (i % 10 == 2))
        return Plural.Two;
      else if ((v == 0) && ((i % 100 == 0) || (i % 100 == 20) || (i % 100 == 40) || (i % 100 == 60) || (i % 100 == 80)))
        return Plural.Few;
      else if ((v != 0))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Arabic, ars
    private static Plural GetArabicPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // zero: n = 0 @integer 0 @decimal 0.0, 0.00, 0.000, 0.0000
      // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
      // two: n = 2 @integer 2 @decimal 2.0, 2.00, 2.000, 2.0000
      // few: n % 100 = 3..10 @integer 3~10, 103~110, 1003, … @decimal 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 103.0, 1003.0, …
      // many: n % 100 = 11..99 @integer 11~26, 111, 1011, … @decimal 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 111.0, 1011.0, …
      // other: @integer 100~102, 200~202, 300~302, 400~402, 500~502, 600, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.1, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 0))
        return Plural.Zero;
      else if ((n == 1))
        return Plural.One;
      else if ((n == 2))
        return Plural.Two;
      else if ((n % 100 >= 3) && (n % 100 <= 10))
        return Plural.Few;
      else if ((n % 100 >= 11) && (n % 100 <= 99))
        return Plural.Many;
      else
        return Plural.Other;
    }

    // Welsh
    private static Plural GetWelshPlural(decimal n, int i, int v, int w, int f, int t)
    {
      // zero: n = 0 @integer 0 @decimal 0.0, 0.00, 0.000, 0.0000
      // one: n = 1 @integer 1 @decimal 1.0, 1.00, 1.000, 1.0000
      // two: n = 2 @integer 2 @decimal 2.0, 2.00, 2.000, 2.0000
      // few: n = 3 @integer 3 @decimal 3.0, 3.00, 3.000, 3.0000
      // many: n = 6 @integer 6 @decimal 6.0, 6.00, 6.000, 6.0000
      // other: @integer 4, 5, 7~20, 100, 1000, 10000, 100000, 1000000, … @decimal 0.1~0.9, 1.1~1.7, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, …
      if ((n == 0))
        return Plural.Zero;
      else if ((n == 1))
        return Plural.One;
      else if ((n == 2))
        return Plural.Two;
      else if ((n == 3))
        return Plural.Few;
      else if ((n == 6))
        return Plural.Many;
      else
        return Plural.Other;
    }

    /// <summary>
    /// Initialize data.
    /// </summary>
    public static void Initialize()
    {
    }

    static PluralData()
    {
      MultiPattern.Register("bm", GetSinglePlural);  // Bamanankan
      MultiPattern.Register("bo", GetSinglePlural);  // Tibetan
      MultiPattern.Register("dz", GetSinglePlural);  // Dzongkha
      MultiPattern.Register("id", GetSinglePlural);  // Indonesian
      MultiPattern.Register("ig", GetSinglePlural);  // Igbo
      MultiPattern.Register("ii", GetSinglePlural);  // Yi
      MultiPattern.Register("in", GetSinglePlural);  // in
      MultiPattern.Register("ja", GetSinglePlural);  // Japanese
      MultiPattern.Register("jbo", GetSinglePlural);  // jbo
      MultiPattern.Register("jv", GetSinglePlural);  // Javanese
      MultiPattern.Register("jw", GetSinglePlural);  // jw
      MultiPattern.Register("kde", GetSinglePlural);  // Makonde
      MultiPattern.Register("kea", GetSinglePlural);  // Kabuverdianu
      MultiPattern.Register("km", GetSinglePlural);  // Khmer
      MultiPattern.Register("ko", GetSinglePlural);  // Korean
      MultiPattern.Register("lkt", GetSinglePlural);  // Lakota
      MultiPattern.Register("lo", GetSinglePlural);  // Lao
      MultiPattern.Register("ms", GetSinglePlural);  // Malay
      MultiPattern.Register("my", GetSinglePlural);  // Burmese
      MultiPattern.Register("nqo", GetSinglePlural);  // N'ko
      MultiPattern.Register("root", GetSinglePlural);  // root
      MultiPattern.Register("sah", GetSinglePlural);  // Sakha
      MultiPattern.Register("ses", GetSinglePlural);  // Koyraboro Senni
      MultiPattern.Register("sg", GetSinglePlural);  // Sango
      MultiPattern.Register("th", GetSinglePlural);  // Thai
      MultiPattern.Register("to", GetSinglePlural);  // Tongan
      MultiPattern.Register("vi", GetSinglePlural);  // Vietnamese
      MultiPattern.Register("wo", GetSinglePlural);  // Wolof
      MultiPattern.Register("yo", GetSinglePlural);  // Yoruba
      MultiPattern.Register("yue", GetSinglePlural);  // yue
      MultiPattern.Register("zh", GetSinglePlural);  // Chinese, Simplified
      MultiPattern.Register("am", GetAmPlural);  // Amharic
      MultiPattern.Register("as", GetAmPlural);  // Assamese
      MultiPattern.Register("bn", GetAmPlural);  // Bangla
      MultiPattern.Register("fa", GetAmPlural);  // Persian
      MultiPattern.Register("gu", GetAmPlural);  // Gujarati
      MultiPattern.Register("hi", GetAmPlural);  // Hindi
      MultiPattern.Register("kn", GetAmPlural);  // Kannada
      MultiPattern.Register("mr", GetAmPlural);  // Marathi
      MultiPattern.Register("zu", GetAmPlural);  // isiZulu
      MultiPattern.Register("ff", GetFfPlural);  // Fulah
      MultiPattern.Register("fr", GetFfPlural);  // French
      MultiPattern.Register("hy", GetFfPlural);  // Armenian
      MultiPattern.Register("kab", GetFfPlural);  // Kabyle
      MultiPattern.Register("ast", GetAstPlural);  // Asturian
      MultiPattern.Register("ca", GetAstPlural);  // Catalan
      MultiPattern.Register("de", GetAstPlural);  // German
      MultiPattern.Register("en", GetAstPlural);  // English
      MultiPattern.Register("et", GetAstPlural);  // Estonian
      MultiPattern.Register("fi", GetAstPlural);  // Finnish
      MultiPattern.Register("fy", GetAstPlural);  // Western Frisian
      MultiPattern.Register("gl", GetAstPlural);  // Galician
      MultiPattern.Register("it", GetAstPlural);  // Italian
      MultiPattern.Register("ji", GetAstPlural);  // ji
      MultiPattern.Register("nl", GetAstPlural);  // Dutch
      MultiPattern.Register("sv", GetAstPlural);  // Swedish
      MultiPattern.Register("sw", GetAstPlural);  // Kiswahili
      MultiPattern.Register("ur", GetAstPlural);  // Urdu
      MultiPattern.Register("yi", GetAstPlural);  // Yiddish
      MultiPattern.Register("si", GetSinhalaPlural);  // Sinhala
      MultiPattern.Register("ak", GetAkPlural);  // Akan
      MultiPattern.Register("bh", GetAkPlural);  // bh
      MultiPattern.Register("guw", GetAkPlural);  // guw
      MultiPattern.Register("ln", GetAkPlural);  // Lingala
      MultiPattern.Register("mg", GetAkPlural);  // Malagasy
      MultiPattern.Register("nso", GetAkPlural);  // Sesotho sa Leboa
      MultiPattern.Register("pa", GetAkPlural);  // Punjabi
      MultiPattern.Register("ti", GetAkPlural);  // Tigrinya
      MultiPattern.Register("wa", GetAkPlural);  // wa
      MultiPattern.Register("tzm", GetCentralAtlasTamazightPlural);  // Central Atlas Tamazight
      MultiPattern.Register("pt", GetPortuguesePlural);  // Portuguese
      MultiPattern.Register("af", GetAfPlural);  // Afrikaans
      MultiPattern.Register("asa", GetAfPlural);  // Asu
      MultiPattern.Register("az", GetAfPlural);  // Azerbaijani, Latin
      MultiPattern.Register("bem", GetAfPlural);  // Bemba
      MultiPattern.Register("bez", GetAfPlural);  // Bena
      MultiPattern.Register("bg", GetAfPlural);  // Bulgarian
      MultiPattern.Register("brx", GetAfPlural);  // Bodo
      MultiPattern.Register("ce", GetAfPlural);  // Chechen
      MultiPattern.Register("cgg", GetAfPlural);  // Chiga
      MultiPattern.Register("chr", GetAfPlural);  // Cherokee
      MultiPattern.Register("ckb", GetAfPlural);  // ckb
      MultiPattern.Register("dv", GetAfPlural);  // Divehi
      MultiPattern.Register("ee", GetAfPlural);  // Ewe
      MultiPattern.Register("el", GetAfPlural);  // Greek
      MultiPattern.Register("eo", GetAfPlural);  // Esperanto
      MultiPattern.Register("es", GetAfPlural);  // Spanish
      MultiPattern.Register("eu", GetAfPlural);  // Basque
      MultiPattern.Register("fo", GetAfPlural);  // Faroese
      MultiPattern.Register("fur", GetAfPlural);  // Friulian
      MultiPattern.Register("gsw", GetAfPlural);  // Swiss German
      MultiPattern.Register("ha", GetAfPlural);  // Hausa
      MultiPattern.Register("haw", GetAfPlural);  // Hawaiian
      MultiPattern.Register("hu", GetAfPlural);  // Hungarian
      MultiPattern.Register("jgo", GetAfPlural);  // Ngomba
      MultiPattern.Register("jmc", GetAfPlural);  // Machame
      MultiPattern.Register("ka", GetAfPlural);  // Georgian
      MultiPattern.Register("kaj", GetAfPlural);  // kaj
      MultiPattern.Register("kcg", GetAfPlural);  // kcg
      MultiPattern.Register("kk", GetAfPlural);  // Kazakh
      MultiPattern.Register("kkj", GetAfPlural);  // Kako
      MultiPattern.Register("kl", GetAfPlural);  // Greenlandic
      MultiPattern.Register("ks", GetAfPlural);  // Kashmiri
      MultiPattern.Register("ksb", GetAfPlural);  // Shambala
      MultiPattern.Register("ku", GetAfPlural);  // Central Kurdish
      MultiPattern.Register("ky", GetAfPlural);  // Kyrgyz
      MultiPattern.Register("lb", GetAfPlural);  // Luxembourgish
      MultiPattern.Register("lg", GetAfPlural);  // Ganda
      MultiPattern.Register("mas", GetAfPlural);  // Masai
      MultiPattern.Register("mgo", GetAfPlural);  // Metaʼ
      MultiPattern.Register("ml", GetAfPlural);  // Malayalam
      MultiPattern.Register("mn", GetAfPlural);  // Mongolian, Cyrillic
      MultiPattern.Register("nah", GetAfPlural);  // nah
      MultiPattern.Register("nb", GetAfPlural);  // Norwegian Bokmål
      MultiPattern.Register("nd", GetAfPlural);  // North Ndebele
      MultiPattern.Register("ne", GetAfPlural);  // Nepali
      MultiPattern.Register("nn", GetAfPlural);  // Norwegian Nynorsk
      MultiPattern.Register("nnh", GetAfPlural);  // Ngiemboon
      MultiPattern.Register("no", GetAfPlural);  // Norwegian
      MultiPattern.Register("nr", GetAfPlural);  // South Ndebele
      MultiPattern.Register("ny", GetAfPlural);  // ny
      MultiPattern.Register("nyn", GetAfPlural);  // Nyankole
      MultiPattern.Register("om", GetAfPlural);  // Oromo
      MultiPattern.Register("or", GetAfPlural);  // Odia
      MultiPattern.Register("os", GetAfPlural);  // Ossetic
      MultiPattern.Register("pap", GetAfPlural);  // Papiamento
      MultiPattern.Register("ps", GetAfPlural);  // Pashto
      MultiPattern.Register("rm", GetAfPlural);  // Romansh
      MultiPattern.Register("rof", GetAfPlural);  // Rombo
      MultiPattern.Register("rwk", GetAfPlural);  // Rwa
      MultiPattern.Register("saq", GetAfPlural);  // Samburu
      MultiPattern.Register("sdh", GetAfPlural);  // sdh
      MultiPattern.Register("seh", GetAfPlural);  // Sena
      MultiPattern.Register("sn", GetAfPlural);  // Shona
      MultiPattern.Register("so", GetAfPlural);  // Somali
      MultiPattern.Register("sq", GetAfPlural);  // Albanian
      MultiPattern.Register("ss", GetAfPlural);  // siSwati
      MultiPattern.Register("ssy", GetAfPlural);  // Saho
      MultiPattern.Register("st", GetAfPlural);  // Sesotho
      MultiPattern.Register("syr", GetAfPlural);  // Syriac
      MultiPattern.Register("ta", GetAfPlural);  // Tamil
      MultiPattern.Register("te", GetAfPlural);  // Telugu
      MultiPattern.Register("teo", GetAfPlural);  // Teso
      MultiPattern.Register("tig", GetAfPlural);  // Tigre
      MultiPattern.Register("tk", GetAfPlural);  // Turkmen
      MultiPattern.Register("tn", GetAfPlural);  // Setswana
      MultiPattern.Register("tr", GetAfPlural);  // Turkish
      MultiPattern.Register("ts", GetAfPlural);  // Tsonga
      MultiPattern.Register("ug", GetAfPlural);  // Uyghur
      MultiPattern.Register("uz", GetAfPlural);  // Uzbek, Latin
      MultiPattern.Register("ve", GetAfPlural);  // Venda
      MultiPattern.Register("vo", GetAfPlural);  // Volapük
      MultiPattern.Register("vun", GetAfPlural);  // Vunjo
      MultiPattern.Register("wae", GetAfPlural);  // Walser
      MultiPattern.Register("xh", GetAfPlural);  // isiXhosa
      MultiPattern.Register("xog", GetAfPlural);  // Soga
      MultiPattern.Register("pt-PT", GetPortuguesePortugalPlural);  // Portuguese (Portugal)
      MultiPattern.Register("da", GetDanishPlural);  // Danish
      MultiPattern.Register("is", GetIcelandicPlural);  // Icelandic
      MultiPattern.Register("mk", GetMacedonianPlural);  // Macedonian
      MultiPattern.Register("fil", GetFilipinoPlural);  // Filipino
      MultiPattern.Register("tl", GetFilipinoPlural);  // tl
      MultiPattern.Register("lv", GetLatvianPlural);  // Latvian
      MultiPattern.Register("prg", GetLatvianPlural);  // Prussian
      MultiPattern.Register("lag", GetLangiPlural);  // Langi
      MultiPattern.Register("ksh", GetColognianPlural);  // Colognian
      MultiPattern.Register("iu", GetIuPlural);  // Inuktitut, Latin
      MultiPattern.Register("kw", GetIuPlural);  // Cornish
      MultiPattern.Register("naq", GetIuPlural);  // Nama
      MultiPattern.Register("se", GetIuPlural);  // Northern Sami
      MultiPattern.Register("sma", GetIuPlural);  // Sami, Southern
      MultiPattern.Register("smi", GetIuPlural);  // smi
      MultiPattern.Register("smj", GetIuPlural);  // Sami, Lule
      MultiPattern.Register("smn", GetIuPlural);  // Sami, Inari
      MultiPattern.Register("sms", GetIuPlural);  // Sami, Skolt
      MultiPattern.Register("shi", GetTachelhitPlural);  // Tachelhit
      MultiPattern.Register("mo", GetRomanianPlural);  // mo
      MultiPattern.Register("ro", GetRomanianPlural);  // Romanian
      MultiPattern.Register("bs", GetBsPlural);  // Bosnian, Latin
      MultiPattern.Register("hr", GetBsPlural);  // Croatian
      MultiPattern.Register("sh", GetBsPlural);  // sh
      MultiPattern.Register("sr", GetBsPlural);  // Serbian, Cyrillic
      MultiPattern.Register("gd", GetScottishGaelicPlural);  // Scottish Gaelic
      MultiPattern.Register("sl", GetSlovenianPlural);  // Slovenian
      MultiPattern.Register("dsb", GetLowerSorbianPlural);  // Lower Sorbian
      MultiPattern.Register("hsb", GetLowerSorbianPlural);  // Upper Sorbian
      MultiPattern.Register("he", GetHebrewPlural);  // Hebrew
      MultiPattern.Register("iw", GetHebrewPlural);  // iw
      MultiPattern.Register("cs", GetCzechPlural);  // Czech
      MultiPattern.Register("sk", GetCzechPlural);  // Slovak
      MultiPattern.Register("pl", GetPolishPlural);  // Polish
      MultiPattern.Register("be", GetBelarusianPlural);  // Belarusian
      MultiPattern.Register("lt", GetLithuanianPlural);  // Lithuanian
      MultiPattern.Register("mt", GetMaltesePlural);  // Maltese
      MultiPattern.Register("ru", GetRussianPlural);  // Russian
      MultiPattern.Register("uk", GetRussianPlural);  // Ukrainian
      MultiPattern.Register("br", GetBretonPlural);  // Breton
      MultiPattern.Register("ga", GetIrishPlural);  // Irish
      MultiPattern.Register("gv", GetManxPlural);  // Manx
      MultiPattern.Register("ar", GetArabicPlural);  // Arabic
      MultiPattern.Register("ars", GetArabicPlural);  // ars
      MultiPattern.Register("cy", GetWelshPlural);  // Welsh
    }
  }
}
