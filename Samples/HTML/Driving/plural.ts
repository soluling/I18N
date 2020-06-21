module Plural
{
  enum PluralKind 
  {
    Default,
    Arabic,
    Czech,
    French,
    Icelandic,
    Irish,
    Japanese,
    Latvian,
    Lithuanian,
    Macedonian,
    Maltese,
    Polish,
    Romanian,
    Russian,
    Slovenian,
    Welsh
  };

  var indexFunction = null;
  var locale: string = "en";

  function GetDefaultIndex(count: number): number
  {
    if (count == 1)
      return 0;
    else
      return 1;
  }

  function GetArabicIndex(count: number): number
  {
    if (count == 0)
      return 0;
    else if (count == 1)
      return 1;
    else if (count == 2)
      return 2;
    else if ((count%100 >= 3) && (count%100 <= 10))
      return 3;
    else if ((count%100 >= 11) && (count%100 <= 99))
      return 4;
    else
      return 5;
  }

  function GetCzechIndex(count: number): number
  {
    if (count == 1)
      return 0;
    else if ((count >= 2) && (count <= 4))
      return 1;
    else
      return 2;
  }

  function GetFrenchIndex(count: number): number
  {
    if (count <= 1)
      return 0;
    else
      return 1;
  }

    function GetIcelandicIndex(count: number): number
    {
      if ((count%10 != 1) || (count%100 == 11))
        return 1;
      else
        return 0;
    }

    function GetIrishIndex(count: number): number
    {
      if (count == 1)
        return 0;
      else if (count == 2)
        return 1;
      else
        return 2;
    }

    function GetJapaneseIndex(count: number): number
    {
      return 0;
    }

    function GetLatvianIndex(count: number): number
    {
      if ((count%10 == 1) && (count%100 != 11))
        return 0;
      else if (count != 0)
        return 1;
      else
        return 2;
    }

    function GetLithuanianIndex(count: number): number
    {
      if ((count%10 == 1) && (count%100 != 11))
        return 0;
      else if ((count%10 == 2) && (count%100 != 12))
        return 1;
      else
        return 2;
    }

    function GetMacedonianIndex(count: number): number
    {
      if (count%10 == 1)
        return 0;
      else if (count%10 == 2)
        return 1;
     else
        return 2;
    }

    function GetMalteseIndex(count: number): number
    {
      if (count == 1)
        return 0;
      else if ((count == 0) || ((count%100 >= 2) && (count%100 <= 10)))
        return 1;
      else if ((count%100 >= 11) && (count%100 < 20))
        return 2;
      else
        return 3;
    }

    function GetPolishIndex(count: number): number
    {
      if (count == 1)
        return 0;
      else if ((count%10 >= 2) && (count%10 <= 4) && (((count%100 < 10) || (count%100 > 20))))
        return 1;
      else
        return 2;
    }

    function GetRomanianIndex(count: number): number
    {
      if (count == 1)
        return 0;
      else if ((count == 0) || ((count%100 >= 1) && (count%100 <= 20)))
        return 1;
      else
        return 2;
    }

    function GetRussianIndex(count: number): number
    {
      if ((count%10 == 1) && (count%100 != 11))
        return 0;
      else if ((count%10 >= 2) && (count%10 <= 4) && ((count%100 < 10) || ((count%100) > 20)))
        return 1;
      else
        return 2;
    }

    function GetSlovenianIndex(count: number): number
    {
      if (count%100 == 1)
        return 0;
      else if (count%100 == 2)
        return 1;
      else if ((count%100 == 3) || (count%100 == 4))
        return 2;
      else
        return 3;
    }

    function GetWelshIndex(count: number): number
    {
      if (count == 1)
        return 0;
      else if (count == 2)
        return 1;
      else if ((count != 8) && (count != 11))
        return 2;
      else
        return 3;
    }

    function isLanguageInArray(language: string, array: string[]): boolean 
    {
      for (var thisLanguage in array)
        if (thisLanguage == language)
          return true;

      return false;
    }

    var englishKind: string[] = 
    [ 
      "en", "de", "nl", "sv", "dk", "no", "nb", "nn", "fi", "et", "fo", "es", "pt", "it", "bg", "el", "eo", "hu", "he", 
      "ab", "aa", "af", "sq", "an", "as", "az", "ba", "eu", "bn", "my", "km", "ca", "tzm", "co", "fy", "ka", "kl", "gu", 
      "ha", "haw", "he", "hi", "kn", "ml",  "mr", "mn", "ne", "se", "pa", "rm", "so", "sw", "tg", "te", "ta", "ur", "uz"
    ];

    var frenchKind: string[] = ["fr", "ak", "am", "br", "fil", "oc"];
    var japaneseKind: string[] = ["ja", "ko", "zh", "vi", "hy", "ay", "chr", "id", "ms", "fa", "syr", "th", "bo", "tr"];
    var russianKind: string[] = ["ru", "be", "uk", "sr", "hr", "bs"];
    var czechKind: string[] = ["cs", "sk"];
    var irishKind: string[] = ["ga", "gd", "dv", "iu"];

    var PROCS: any[] = 
    [
      GetDefaultIndex,
      GetArabicIndex,
      GetCzechIndex,
      GetFrenchIndex,
      GetIcelandicIndex,
      GetIrishIndex,
      GetJapaneseIndex,
      GetLatvianIndex,
      GetLithuanianIndex,
      GetMacedonianIndex,
      GetMalteseIndex,
      GetPolishIndex,
      GetRomanianIndex,
      GetRussianIndex,
      GetSlovenianIndex,
      GetWelshIndex
    ];

    function GetKind(locale: string): PluralKind
    {
      var language: string = "en";
      var country: string = "";
      var index: number = locale.indexOf("-");

      if (index > 0)
      {
        language = locale.substr(0, index);
        country = locale.substr(index + 1, locale.length);
      }
      else
      {
        language = locale;
        country = "";
      }

      if (isLanguageInArray(language, frenchKind) || ((language == "pt") && (country == "BR")))
        return PluralKind.French;
      else if (isLanguageInArray(language, englishKind))
        return PluralKind.Default;
      else if (isLanguageInArray(language, japaneseKind))
        return PluralKind.Japanese;
      else if (isLanguageInArray(language, russianKind))
        return PluralKind.Russian;
      else if (isLanguageInArray(language, czechKind))
        return PluralKind.Czech;
      else if (isLanguageInArray(language, irishKind))
        return PluralKind.Irish;
      else if (language == "ar")
        return PluralKind.Arabic;
      else if (language == "is")
        return PluralKind.Icelandic;
      else if (language == "lv")
        return PluralKind.Latvian;
      else if (language == "lt")
        return PluralKind.Lithuanian;
      else if (language == "mk")
        return PluralKind.Macedonian;
      else if (language == "mt")
        return PluralKind.Maltese;
      else if (language == "pl")
        return PluralKind.Polish;
      else if (language == "ro")
        return PluralKind.Romanian;
      else if (language == "sl")
        return PluralKind.Slovenian;
      else if (language == "cy")
        return PluralKind.Welsh;
      else
        return PluralKind.Default;
    }

  export function GetIndexProc(locale: string)
  {
    return PROCS[<number>GetKind(locale)];
  }

  function ParsePattern(pattern: string): string[]
  {
    var items: string[] = new Array();
    var i = 0;

    while (i < pattern.length)
    {
      var str: string = "";

      while (i < pattern.length)
      {
        var c: string = pattern.charAt(i);

        if (c == ';')
        {
          i++;
          break;
        }
        else if ((c == '\\') && (i < pattern.length - 1))
        {
          i++;
          c = pattern.charAt(i);

          if (c == ';')
            c = ';';
        }

        str = str + c;
        i++;
      }

      items.push(str);
    }

    return items;
  }

  export function getPattern(format: string, count: number): string
  {
    if (indexFunction == null)
      indexFunction = GetIndexProc(locale);

    var index: number = indexFunction(count);
    var parts: string[] = ParsePattern(format);

    if (index < parts.length)
      return parts[index];
    else
      return parts[parts.length - 1];
  }

  export function setPluralLanguage(value: string)
  {
    locale = value;
  }

  declare var sprintf;

  export function pluralSprintf(pattern: string, count: number, ...args: any[])
  {
    var newArgs: any[] = new Array();

    newArgs.push(getPattern(pattern, count));

    for (var i = 0; i < args.length; i++)
      newArgs.push(args[i]);

    return sprintf.apply(undefined, newArgs);
  }
}