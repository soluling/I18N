import { Language, PluralForm, Gender } from "./language";

/**
 * Enumeration that specifies what plural group where language belongs to.
 * The algorithm is named based on the main language of the group.
 * For example PluralGroup.Japanese is not only applied to Japanese but all the other languages where plural rules are the same as in Japanese (expecially Asian ones).
 */
export enum PluralGroup
{
  Default,     // Language group that behaves like English. 1 uses singular. All other including 0 use plural.
  Arabic,      // Language group that behaves like Arabic. Uses all six plural forms.
  Czech,       // Language group that behaves like Czech. 1 uses singular. 2-4 use paucal. All other including 0 use plural.
  French,      // Language group that behaves like French. 0 and 1 use singular. All other use plural.
  Icelandic,   // Language group that behaves like Icelandic. 1 and everything ending with 1 except those ending with 11 are singular. All other including 0 use plural.
  Irish,       // Language group that behaves like Irish. 1 uses singular. 2 uses dual.  All other including 0 use plural.
  Japanese,    // Language group that behaves like Japanese. Everything is singular. There are no plural forms.
  Latvian,     // Language group that behaves like Latvian.
  Lithuanian,  // Language group that behaves like Lithuanian.
  Macedonian,  // Language group that behaves like Macedonian.
  Maltese,     // Language group that behaves like Maltese.
  Polish,      // Language group that behaves like Polish.
  Romanian,    // Language group that behaves like Romanian.
  Russian,     // Language group that behaves like Russian.
  Slovenian,   // Language group that behaves like Slovenian.
  Welsh        // Language group that behaves like Welsh.
};

class Pattern
{
  /** Pattern string. */
  Value: string;

  /**
   * Create a pattern
   */
  constructor(value: string)
  {
    this.Value = value;
  }
}

/***
 * Specifies a single plural pattern. It contains the pattern string and the plural form where it is used.
 */
class PluralPattern extends Pattern
{
  /** Plural form of the pattern. */
  Form: PluralForm;

  /**
   * Create a plural pattern.
   * @param {string} value - The pattern string.
   * @param {PluralForm} form - The plural form where this pattern is used.
   */
  constructor(value: string, form: PluralForm)
  {
    super(value)
    this.Form = form;
  }
}

/***
 * Specifies a single gender pattern. It contains the pattern string and the gender value where it is used.
 */
class GenderPattern extends Pattern
{
  /** Gender of the pattern. */
  Gender: Gender;

  /**
   * Create a gender pattern.
   * @param {string} value - The pattern string.
   * @param {Gender} form - The gender when this pattern is used.
   */
  constructor(value: string, gender: Gender)
  {
    super(value)
    this.Gender = gender;
  }
}

/**
 * Part of the message pattern. Generally contains one plural or gender placeholder. Contain one or more patterns - one for each pluar or gender forms.
 */
class FormatPart
{
  items: Pattern[] = [];

  /**
   * Get the pattern count.
   * @return {number} The pattern count. 
   */
  get Count(): number
  {
    return this.items.length;
  }

  /**
   * Get the pattern for Plural.Other.
   * @return {string} The other pattern. 
   */
  get OtherValue(): string
  {
    return this.GetPluralString(PluralForm.Other);
  }

  /**
   * Get the first pattern.
   * @return {string} The first pattern. 
   */
  get DefaultValue(): string
  {
    if (this.Count > 0)
      return this.GetPattern(0).Value;
    else
      return "";
  }

  /**
   * Check if the part is a plural part.
   * @return {boolean} Return true if this part is a plural part. 
   */
  get IsPlural(): boolean
  {
    for (var i = 0; i < this.Count; i++)
    {
      var pattern = this.GetPattern(i);

      if (pattern instanceof PluralPattern)
        return true;
    }

    return false;
  }

  /**
   * Check if the part is a gender part.
   * @return {boolean} Return true if this part is a gender part. 
   */
  get IsGender(): boolean
  {
    for (var i = 0; i < this.Count; i++)
    {
      var pattern = this.GetPattern(i);

      if (pattern instanceof GenderPattern)
        return true;
    }

    return false;
  }

  /**
   * Get a pattern.
   * @param {number} index The index of the pattern
   * @return {Pattern} The pattern.
   */
  public GetPattern(index: number): Pattern
  {
    return this.items[index];
  }

  /**
   * Get a pattern string.
   * @param {PluralForm} form The plural form that is used.
   * @param {number} count The count where the pattern will be used.
   * @return {string} The pattern string.
   */
  public GetCheckedPluralString(form: PluralForm, count: number): string
  {
    if (!this.IsPlural)
      throw new Error("Pattern is not a plural pattern");

    var pattern: string;

    if ((count == 0) && this.PluralExists(PluralForm.Zero))
      pattern = this.GetPluralString(PluralForm.Zero);
    else if ((form == PluralForm.Other) && (count == 1) && this.PluralExists(PluralForm.One))
      pattern = this.GetPluralString(PluralForm.One);
    else if ((form == PluralForm.Other) && (count == 2) && this.PluralExists(PluralForm.Two))
      pattern = this.GetPluralString(PluralForm.Two);
    else
      pattern = this.GetPluralString(form);

    if (pattern == "")
      pattern = this.DefaultValue;
    
    return pattern;
  }

  public GetCheckedGenderString(gender: Gender): string
  {
    if (!this.IsGender)
      throw new Error("Pattern is not a gender pattern");

    var pattern = this.GetGenderString(gender);

    if (pattern == "")
      pattern = this.DefaultValue;

    return pattern;
  }

  public GetPluralString(form: PluralForm): string
  {
    var item: PluralPattern = this.FindPlural(form);

    if (item != null)
      return item.Value;
    else
      return "";
  }

  public SetPluralString(form: PluralForm, value: string)
  {
    var item: PluralPattern = this.FindPlural(form);

    if (item != null)
      item.Value = value;
    else
      this.AddPlural(value, form);
  }

  public GetGenderString(gender: Gender): string
  {
    var item: GenderPattern = this.FindGender(gender);

    if (item != null)
      return item.Value;
    else
      return "";
  }

  public SetGenderString(gender: Gender, value: string)
  {
    var item: GenderPattern = this.FindGender(gender);

    if (item != null)
      item.Value = value;
    else
      this.AddGender(value, gender);
  }

  public FindPlural(form: PluralForm): PluralPattern
  {
    for (var pattern of this.items)
      if ((pattern instanceof PluralPattern) && (pattern.Form == form))
        return pattern;

    return null;
  }

  public FindGender(gender: Gender): GenderPattern
  {
    for (var pattern of this.items)
      if ((pattern instanceof GenderPattern) && (pattern.Gender == gender))
        return pattern;

    return null;
  }

  public PluralExists(form: PluralForm): boolean
  {
    return this.FindPlural(form) != null;
  }

  public GenderExists(gender: Gender): boolean
  {
    return this.FindGender(gender) != null;
  }

  public AddPlural(value: string, form: PluralForm): PluralPattern
  {
    var item: PluralPattern = new PluralPattern(value, form);
    this.items.push(item);
    return item;
  }

  public AddGender(value: string, gender: Gender): GenderPattern
  {
    var item: GenderPattern = new GenderPattern(value, gender);
    this.items.push(item);
    return item;
  }
}

const NEXT: string  = "next";

/**
 * 
 */
export class FormatString
{
  private items: FormatPart[] = [];
  public StartPattern: string= "";

  constructor(pattern: string)
  {
    this.ParsePatterns(pattern);
  }

  get Count(): number
  {
    return this.items.length;
  }

  public GetItem(index: number): FormatPart
  {
    return this.items[index];
  }

  public Find(form: PluralForm, all: boolean = true): PluralPattern
  {
    var result: PluralPattern;

    if (all)
    {
      for (var i = 0; i < this.Count; i++)
      {
        result = this.GetItem(i).FindPlural(form);

        if (result != null)
          return result;
      }

      return null;
    }
    else
      return this.GetItem(0).FindPlural(form);
  }

  public AddParameter(): FormatPart
  {
    var result: FormatPart = new FormatPart();
    this.items.push(result);
    return result;
  }

  private ParsePatterns(patterns: string)
  {
    var formStr: string = "";
    var genderStr: string = "";
    var parameter: FormatPart = this.AddParameter();
    var items: string[] = new Array();
    var i = 0;

    while (i < patterns.length)
    {
      var str: string = "";

      while (i < patterns.length)
      {
        var c: string = patterns.charAt(i);

        if (c == ';')
        {
          i++;

          if ((i >= patterns.length) || (patterns.charAt(i) != ';'))
            break;
        }

        str = str + c;
        i++;
      }

      if (str == NEXT)
      {
        // New parameter
        parameter = this.AddParameter();
      }
      else if (formStr != "")
      {
        // Add plural pattern
        parameter.AddPlural(str, Plural.StringToPlural(formStr));
        formStr = "";
      }
      else if (genderStr != "")
      {
        // Add gender pattern
        parameter.AddGender(str, Plural.StringToGender(genderStr));
        genderStr = "";
      }
      else if (Plural.IsPluralForm(str))
      {
        // Plural pattern follows
        formStr = str;
      }
      else if (Plural.IsGender(str))
      {
        // Gender pattern follows
        genderStr = str;
      }
      else if (this.StartPattern == "")
        this.StartPattern = str;
      else
        throw new Error("Invalid pattern: " + patterns);
    }
  }
}

class FormData
{
  id: string;
  num: number;
}

/**
 * Implements a plural enabled string format functions.
 */
export class Plural extends Language
{
  protected static indexProc = null;

  // Size and order must match PluralForm
  private static FORM_DATAS: FormData[] =
  [
    { id: "zero", num: 0 },
    { id: "one", num: 1 },
    { id: "two", num: 2 },
    { id: "few", num: -1 },
    { id: "many", num: -1 },
    { id: "other", num: -1 }
  ];

  // Size and order must match Gender
  private static GENDER_DATAS: string[] =
  [
    "male",
    "female",
    "neutral"
  ];

  // Size and order must match PluralGroup
  private static PROCS: any[] = 
  [
    Plural.GetDefaultIndex,
    Plural.GetArabicIndex,
    Plural.GetCzechIndex,
    Plural.GetFrenchIndex,
    Plural.GetIcelandicIndex,
    Plural.GetIrishIndex,
    Plural.GetJapaneseIndex,
    Plural.GetLatvianIndex,
    Plural.GetLithuanianIndex,
    Plural.GetMacedonianIndex,
    Plural.GetMalteseIndex,
    Plural.GetPolishIndex,
    Plural.GetRomanianIndex,
    Plural.GetRussianIndex,
    Plural.GetSlovenianIndex,
    Plural.GetWelshIndex
  ];

  private static englishKind: string[] = 
  [ 
    "en", "de", "nl", "sv", "dk", "no", "nb", "nn", "fi", "et", "fo", "es", "pt", "it", "bg", "el", "eo", "hu", "he", 
    "ab", "aa", "af", "sq", "an", "as", "az", "ba", "eu", "bn", "my", "km", "ca", "tzm", "co", "fy", "ka", "kl", "gu", 
    "ha", "haw", "he", "hi", "kn", "ml",  "mr", "mn", "ne", "se", "pa", "rm", "so", "sw", "tg", "te", "ta", "ur", "uz"
  ];

  private static frenchKind: string[] = ["fr", "ak", "am", "br", "fil", "oc"];
  private static japaneseKind: string[] = ["ja", "ko", "zh", "vi", "hy", "ay", "chr", "id", "ms", "fa", "syr", "th", "bo", "tr"];
  private static russianKind: string[] = ["ru", "be", "uk", "sr", "hr", "bs"];
  private static czechKind: string[] = ["cs", "sk"];
  private static irishKind: string[] = ["ga", "gd", "dv", "iu"];

  private static GetDefaultIndex(count: number): PluralForm
  {
    if (count == 1)
      return PluralForm.One;
    else
      return PluralForm.Other;
  }

  private static GetArabicIndex(count: number): PluralForm
  {
    if (count == 0)
      return PluralForm.Zero;
    else if (count == 1)
      return PluralForm.One;
    else if (count == 2)
      return PluralForm.Two;
    else if ((count%100 >= 3) && (count%100 <= 10))
      return PluralForm.Few;
    else if ((count%100 >= 11) && (count%100 <= 99))
      return PluralForm.Many;
    else
      return PluralForm.Other;
  }

  private static GetCzechIndex(count: number): PluralForm
  {
    if (count == 1)
      return PluralForm.One;
    else if ((count >= 2) && (count <= 4))
      return PluralForm.Few;
    else
      return PluralForm.Other;
  }

  private static GetFrenchIndex(count: number): PluralForm
  {
    if (count <= 1)
      return PluralForm.One;
    else
      return PluralForm.Other;
  }

  private static GetIcelandicIndex(count: number): PluralForm
  {
    if ((count%10 != 1) || (count%100 == 11))
      return PluralForm.Other;
    else
      return PluralForm.One;
  }

  private static GetIrishIndex(count: number): PluralForm
  {
    if (count == 1)
      return PluralForm.One;
    else if (count == 2)
      return PluralForm.Two;
    else
      return PluralForm.Other;
  }

  private static GetJapaneseIndex(count: number): PluralForm
  {
    return PluralForm.Other;
  }

  private static GetLatvianIndex(count: number): PluralForm
  {
    if ((count%10 == 1) && (count%100 != 11))
      return PluralForm.One;
    else if (count != 0)
      return PluralForm.Zero;
    else
      return PluralForm.Other;
  }

  private static GetLithuanianIndex(count: number): PluralForm
  {
    if ((count%10 == 1) && (count%100 != 11))
      return PluralForm.One;
    else if ((count%10 == 2) && (count%100 != 12))
      return PluralForm.Few;
    else
      return PluralForm.Other;
  }

  private static GetMacedonianIndex(count: number): PluralForm
  {
    if (count%10 == 1)
      return PluralForm.One;
    else if (count%10 == 2)
      return PluralForm.Two;
    else
      return PluralForm.Other;
  }

  private static GetMalteseIndex(count: number): PluralForm
  {
    if (count == 1)
      return PluralForm.One;
    else if ((count == 0) || ((count%100 >= 2) && (count%100 <= 10)))
      return PluralForm.Few;
    else if ((count%100 >= 11) && (count%100 < 20))
      return PluralForm.Many;
    else
      return PluralForm.Other;
  }

  private static GetPolishIndex(count: number): PluralForm
  {
    if (count == 1)
      return PluralForm.One;
    else if ((count%10 >= 2) && (count%10 <= 4) && (((count%100 < 10) || (count%100 > 20))))
      return PluralForm.Few;
    else
      return PluralForm.Other;
  }

  private static GetRomanianIndex(count: number): PluralForm
  {
    if (count == 1)
      return PluralForm.One;
    else if ((count == 0) || ((count%100 >= 1) && (count%100 <= 20)))
      return PluralForm.Few;
    else
      return PluralForm.Other;
  }

  private static GetRussianIndex(count: number): PluralForm
  {
    if ((count%10 == 1) && (count%100 != 11))
      return PluralForm.One;
    else if ((count%10 >= 2) && (count%10 <= 4) && ((count%100 < 10) || ((count%100) > 20)))
      return PluralForm.Few;
    else
      return PluralForm.Other;
  }

  private static GetSlovenianIndex(count: number): PluralForm
  {
    if (count%100 == 1)
      return PluralForm.One;
    else if (count%100 == 2)
      return PluralForm.Two;
    else if ((count%100 == 3) || (count%100 == 4))
      return PluralForm.Few;
    else
      return PluralForm.Other;
  }

  private static GetWelshIndex(count: number): PluralForm
  {
    if (count == 0)
      return PluralForm.Zero;
    else if (count == 1)
      return PluralForm.One;
    else if (count == 2)
      return PluralForm.Two;
    else if (count == 3)
      return PluralForm.Few;
    else if (count == 6)
      return PluralForm.Many;
    else
      return PluralForm.Other;
  }

  private static DoStringToPlural(value: string): [boolean, PluralForm]
  {
    for (var i = 0; i < this.FORM_DATAS.length; i++)
    {
      var formData = this.FORM_DATAS[i];

      if ((value == formData.id) || ((formData.num >= 0) && ((value == formData.num.toString()) || (value == "=" + formData.num.toString()))))
        return [true, <PluralForm>i];
    }

    return [false, PluralForm.Other];
  }

  /**
   * Convert plural form code into a plural code.
   * @param {string} value The plural form code such as "one".
   * @return The plural code.
   */
  public static StringToPlural(value: string): PluralForm
  {
    return this.DoStringToPlural(value)[1];
  }

  /**
   * Check if the passed string is a plural form code.
   * @param {string} value The plural form code to be checked such as "one".
   * @return True if the passed string is a valid plural form code.
   */
  public static IsPluralForm(value: string): boolean
  {
    return this.DoStringToPlural(value)[0];
  }

  private static DoStringToGender(value: string): [boolean, Gender]
  {
    for (var i = 0; i < this.GENDER_DATAS.length; i++)
    {
      if (value == this.GENDER_DATAS[i])
        return [true, <Gender>i];
    }

    return [false, Gender.Neutral];
  }

  /**
   * Convert gender code into a gender.
   * @param {string} value The gender code such as "male".
   * @return The gender.
   */
  public static StringToGender(value: string): Gender
  {
    return this.DoStringToGender(value)[1];
  }

  /**
   * Check if the passed string is a gender code.
   * @param {string} value The gender code to be checked such as "male".
   * @return True if the passed string is a valid gender code.
   */
  public static IsGender(value: string): boolean
  {
    return this.DoStringToGender(value)[0];
  }

  private static IsLanguageInArray(language: string, array: string[]): boolean 
  {
    for (var thisLanguage of array)
      if (thisLanguage == language)
        return true;

    return false;
  }

  private static GetKind(locale: string): PluralGroup
  {
    var language: string = "en";
    var country: string = "";
    var ids: string[] = locale.split("-");

    language = ids[0];

    if (ids.length > 1)
      country = ids[1];

    if (this.IsLanguageInArray(language, this.frenchKind) || ((language == "pt") && (country == "BR")))
      return PluralGroup.French;
    else if (this.IsLanguageInArray(language, this.englishKind))
      return PluralGroup.Default;
    else if (this.IsLanguageInArray(language, this.japaneseKind))
      return PluralGroup.Japanese;
    else if (this.IsLanguageInArray(language, this.russianKind))
      return PluralGroup.Russian;
    else if (this.IsLanguageInArray(language, this.czechKind))
      return PluralGroup.Czech;
    else if (this.IsLanguageInArray(language, this.irishKind))
      return PluralGroup.Irish;
    else if (language == "ar")
      return PluralGroup.Arabic;
    else if (language == "is")
      return PluralGroup.Icelandic;
    else if (language == "lv")
      return PluralGroup.Latvian;
    else if (language == "lt")
      return PluralGroup.Lithuanian;
    else if (language == "mk")
      return PluralGroup.Macedonian;
    else if (language == "mt")
      return PluralGroup.Maltese;
    else if (language == "pl")
      return PluralGroup.Polish;
    else if (language == "ro")
      return PluralGroup.Romanian;
    else if (language == "sl")
      return PluralGroup.Slovenian;
    else if (language == "cy")
      return PluralGroup.Welsh;
    else
      return PluralGroup.Default;
  }

  /**
   * Get the plural index function.
   * @param {string} locale The locale whose plural index function to get.
   * @return {any} The plural index function.
   */
  public static GetIndexProc(locale: string)
  {
    return Plural.PROCS[<number>this.GetKind(locale)];
  }

  /**
   * Initialize the plural index function for the active language if not already initialized.
   */
  protected static InitializeIndexProc(): void
  {
    if (Plural.indexProc == null)
      Plural.indexProc = this.GetIndexProc(this.locale);
  }

  /**
   * Get the pattern matching the specified count.
   * @param {string} patterns The multi pattern string.
   * @param {number} count The count.
   * @return {string} The pattern.  
   */
  public static GetPluralPattern(patterns: string, count: number): string
  {
    this.InitializeIndexProc();
    var form: PluralForm = Plural.indexProc(count);

    return new FormatString(patterns).GetItem(0).GetCheckedPluralString(form, count);
  }

  /**
   * Get the pattern matching the specified gender.
   * @param {string} patterns The multi pattern string.
   * @param {Gender} gender The gender.
   * @return {string} The pattern.  
   */
  public static GetGenderPattern(patterns: string, gender: Gender): string
  {
    return new FormatString(patterns).GetItem(0).GetCheckedGenderString(gender);
  }

  /**
   * Interpolates the pattern with the parameters.
   * @param {string} pattern - The pattern string.
   * @param {any} data - Arguments for the interpolation pattern. Either single object or an array of objects.
   * @return {string} The result string where parameter(s) were interpolated with the pattern.  
   */
  public static Interpolate(pattern: string, data: any): string 
  {
    return pattern.replace(
      /\{\{ *([\w_]+) *\}\}/g, 
      function (str, key) 
      {
        var value: any;

        if (Array.isArray(data))
        {
          var args: any[] = data;

          for (var arg of args)
          {
            value = arg[key];

            if (value != undefined)
              break; 
          };
        }
        else
        {
          value = data[key];
        }
        
        if (value === undefined) 
          throw new Error('No value provided for variable ' + str);
        else if (typeof value === 'function') 
          value = value(data);

        return value;
      }
    );
  }

  /**
   * Selects the right pattern from the passed multi pattern string and applies that for interpolation.
   * @param {string} patterns - The multi pattern string.
   * @param {any} args - Arguments for the interpolation pattern.
   * @return {string} The result string where parameter(s) were interpolated with the pattern.  
   */
  public static FormatPlural(patterns: string, args: any)
  {
    var count: number = 1;

    if (typeof args == "number")
      count = args
    else
    {
      for (var argKey in args)
      {
        count = args[argKey];
        break;
      }
    }  

    var pattern: string = this.GetPluralPattern(patterns, count);

    return this.Interpolate(pattern, args);
  }

  /**
   * Selects the right gender pattern from the passed multi pattern string and applies that for interpolation.
   * @param {string} patterns - The multi pattern string.
   * @param {Gender} gender - Gender value.
   * @param {any} args - Optional arguments for interpolation.
   * @return {string} The result string where parameter(s) were interpolated with the pattern.  
   */
  public static FormatGender(patterns: string, gender: Gender, ...args: any[]): string
  {
    var pattern = this.GetGenderPattern(patterns, gender);
    return this.Interpolate(pattern, args);
  }

  /**
   * Selects the right gender pattern from the passed multi pattern string and applies that for interpolation.
   * @param {string} patterns - The multi pattern string.
   * @param {any} args - Arguments for interpolation. For plural parameter pass an object having a single number property. For gender parameter pass an object containing gender and value properties.
   * @return {string} The result string where parameter(s) were interpolated with the pattern.  
   */
  public static Format(patterns: string, ...args: any[]): string
  {
    this.InitializeIndexProc();

    var result: string = "";
    var str: FormatString = new FormatString(patterns);
    var count: number = args.length;

    if (count != str.Count)
      throw new Error("Pattern " + patterns + " does not have enough patterns");

    // Process parameter from last to first and build the result string part by part
    for (var i = str.Count - 1; i >= 0; i--)
    {
      var parameter = str.GetItem(i);
      var arg = args[i];
      var keys = Object.keys(arg);
      var pattern: string;

      if (arg["gender"] != undefined)
      {
        // Gender
        var gender: Gender = arg["gender"];
        pattern = parameter.GetCheckedGenderString(gender);
      } 
      else if (keys.length > 0)
      {
        // Plural
        var count: number = arg[keys[0]];
        var form: PluralForm = Plural.indexProc(count);
        pattern = parameter.GetCheckedPluralString(form, count);
      }
      else
        throw new Error("No parameters for part #" + i);

      if (i < str.Count - 1)
        arg["next"] = result;

      result = this.Interpolate(pattern, arg);
    }

    if (str.StartPattern != "")
      result = this.Interpolate(str.StartPattern, result);

    return result;
  }
}