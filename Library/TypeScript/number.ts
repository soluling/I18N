import { Language, PluralForm } from "./language";

export enum AbbreviatedNumberForm
{
  Short,
  Long,
  Currency
};

class AbbreviationRule
{
  Range: number;
  Plural: PluralForm;
  Value: string;
}

class Data
{
  Short: AbbreviationRule[];
  Long: AbbreviationRule[];
  Currency: AbbreviationRule[];
}

/*
class ReverseComparer : IComparer<AbbreviationRule>
{
     public int Compare(AbbreviationRule rule1, AbbreviationRule rule2)  
     {
      if (rule1.Range == rule2.Range)
        return (new CaseInsensitiveComparer()).Compare(rule2.Plural, rule1.Plural);
      else
        return (new CaseInsensitiveComparer()).Compare(rule2.Range, rule1.Range);
     }
  }
*/

export class AbbreviatedNumber extends Language
{
  private static datas;
  private static DEFAULT_PRECISION = 2;

  public static Format(
    form: AbbreviatedNumberForm,
    value: number,
    precision: number = AbbreviatedNumber.DEFAULT_PRECISION): string
  {
    switch (form)
    {
      case AbbreviatedNumberForm.Short:
        return this.FormatShort(value, precision);

      case AbbreviatedNumberForm.Currency:
        return this.FormatCurrency(value, precision);

      default:
        return this.FormatLong(value, precision);
    }
  }

  public static FormatLong(
    value: number, 
    precision: number = AbbreviatedNumber.DEFAULT_PRECISION): string
  {
    return DoFormat(value, AbbreviatedNumberForm.Long, precision, this.GetData().Long);
  }

  public static FormatShort(
    value: number, 
    precision: number = AbbreviatedNumber.DEFAULT_PRECISION): string
  {
    return DoFormat(value, AbbreviatedNumberForm.Short, precision, this.GetData().Short);
  }

  public static FormatCurrency(
    value: number, 
    precision: number = AbbreviatedNumber.DEFAULT_PRECISION): string
  {
    return DoFormat(value, AbbreviatedNumberForm.Currency, precision, this.GetData().Currency);
  }

  public static Register(id: string, longRules: AbbreviationRule[], shortRules: AbbreviationRule[], currencyRules: AbbreviationRule[])
  {
    var data: Data = null;

    if (this.datas[id] == null)
    {
      data = new Data();
      this.datas.Add(id, data);
    }

    data.Long = this.Sort(longRules);
    data.Short = this.Sort(shortRules);
    data.Currency = this.Sort(currencyRules);
  }

  private static Sort(rules: AbbreviationRule[]): AbbreviationRule[]
  {
    var result = rules.slice();

    result.sort((rule1: AbbreviationRule, rule2: AbbreviationRule) => 
    {
      if (rule1.Range < rule2.Range)
        return 1;
      else if (rule1.Range > rule2.Range)
        return -1;
      else if (rule1.Plural < rule2.Plural)
        return 1;
      else if (rule1.Plural > rule2.Plural)
        return -1;
      else
        return 0;
    });

    return result;
  }

  private static GetValuePrecision(value: number): number
  {
    var result = 0;
    var valueRange = 1;

    while (valueRange <= value)
    {
      valueRange *= 10;
      result++;
    }

    return result;
  }

  private static RoundToSignificantDigits(value: number, digits: number): number
  {
    if (value == 0)
      return value;

    return parseFloat(value.toFixed(digits));
  }

  private static DeleteTrailingZeros(value: string): string
  {
    var decimalSeparator = this.GetDecimalSeparator();

    if (value.indexOf(decimalSeparator) == -1)
      return value;

    var result = value;

    var i = result.length - 1;

    while ((i > 0) && !((result[i].toString() == decimalSeparator) || ((result[i] >= '0') && (result[i] <= '9'))))
      i--;

    while ((i >= 0) && (result[i] == '0'))
    {
      result = result.slice(0, i - 1) + result.slice(i + 1);
      i--;
    }

    if ((i >= 0) && (result[i].toString() == decimalSeparator))
      result = result.slice(0, i - 1) + result.slice(i + 1);

    return result;
  }

  private static GetStr(value: number, precision: number): string
  {
    var valuePrecision = this.GetValuePrecision(value);
    value = this.RoundToSignificantDigits(value, precision);

    var result = value.toLocaleString(
      this.locale, 
      { maximumSignificantDigits: Math.max(0, precision - valuePrecision) });

    return this.DeleteTrailingZeros(result);
  }

  private static GetNotAbbreviatedStr(value: number, form: AbbreviatedNumberForm, precision: number): string
  {
    if (form == AbbreviatedNumberForm.Currency)
    {
      //var valuePrecision = this.GetValuePrecision(value);
      //value = this.RoundToSignificantDigits(value, precision);

      var result: string;

      if (value == Math.round(value))
        result = new Intl.NumberFormat(this.locale, { style:"currency", maximumSignificantDigits: precision, maximumFractionDigits: 0 }).format(value);
      else
        result = new Intl.NumberFormat(this.locale, { style:"currency", maximumSignificantDigits: precision }).format(value);

      return this.DeleteTrailingZeros(result);
    }
    else
      return this.GetStr(value, precision);
  }

  private static Find(range: number, plural: PluralForm, rules: AbbreviationRule[]): AbbreviationRule
  {
    rules.forEach(rule => 
    { 
      if ((rule.Range == range) && (rule.Plural == plural)) 
        return rule; 
    });

    return null;
  }

  private static DoFormat(value: number, form: AbbreviatedNumberForm, precision: number, rules: AbbreviationRule[]): string
  {
    foreach (var rule in rules)
    {
      if (rule.Range <= value)
      {
        var range = rule.Range;
        var result = rule.Value;
        var p = result.IndexOf('0');
        result = result.Remove(p, 1);

        while ((result != "") && (result[p] == '0'))
        {
          result = result.Remove(p, 1);
          range = range / 10;
        }

        string str;
        double displayValue;

        if (result != "")
        {
          str = GetStr(value / range, precision);
          displayValue = Convert.ToDouble(str);
        }
        else
        {
          str = GetNotAbbreviatedStr(value, form, precision);
          displayValue = value;
        }

        uint count = (uint)Math.Round(displayValue);

        if (displayValue == count)
        {
          var proc = Plural.GetIndexProc();
          var plural = proc(count);

          if (plural != rule.Plural)
          {
            // Wrong plural form. Find the correct one and get abbreviation again
            var newRule = Find(rule.Range, plural, rules);

            if (newRule != null)
            {
              range = newRule.Range;
              result = rule.Value;
              p = result.IndexOf('0');
              result = result.Remove(p, 1);

              while ((result != "") && (result[p] == '0'))
              {
                result = result.Remove(p, 1);
                range = range / 10;
              }

              str = GetStr(value/range, precision);
            }
          }
        }

        result = result.Substring(0, p) + str + result.Substring(p);
        p = result.IndexOf('¤');

        if (p >= 0)
          result = result.Substring(0, p) + NumberFormatInfo.CurrentInfo.CurrencySymbol + result.Substring(p + 1);

        return result;
      }
    }

    return GetNotAbbreviatedStr(value, form, precision);
  }

  private static GetData(): Data
  {
    AbbreviatedNumberData.Initialize();

    var id = this.locale;

    if (!this.datas.ContainsKey(id))
    {
      id = CultureInfo.CurrentUICulture.TwoLetterISOLanguageName;

      if (!this.datas.ContainsKey(id))
        id = "";
    }

    return this.datas[id];
  }

  private static readonly NULL_RULES: AbbreviationRule[] = new AbbreviationRule[0];

  static AbbreviatedNumber()
  {
    this.Register("", this.NULL_RULES, this.NULL_RULES, this.NULL_RULES);
  }
}
