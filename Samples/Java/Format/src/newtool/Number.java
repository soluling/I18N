package newtool;

import java.util.*;

enum AbbreviatedNumberForm
{
  Long,
  Short,
  Currency
}

class AbbreviationRule
{
  long Range;
  PluralForm Plural;
  String Value;
}

class NumberData
{
  AbbreviationRule[] Long;
  AbbreviationRule[] Short;
  AbbreviationRule[] Currency;
}

class ReverseComparer implements Comparator<AbbreviationRule>
{
  public int compare(AbbreviationRule rule1, AbbreviationRule rule2)  
  {
    if (rule1.Range < rule2.Range)
      return 1;
    else if (rule1.Range > rule2.Range)
      return -1;
    else if (rule1.Plural.ordinal() < rule2.Plural.ordinal())
      return 1;
    else if (rule1.Plural.ordinal() > rule2.Plural.ordinal())
      return -1;
    else
      return 0;
  }
}

static class AbbreviatedNumber
{
  private static Map<String, NumberData> datas = new HashMap<String, NumberData>();

  static final int DEFAULT_PRECISION = 2;

  public static String Format(
    AbbreviatedNumberForm form,
    double value,
    int precision)
  {
    switch (form)
    {
      case Short:
        return FormatShort(value, precision);

      case Currency:
        return FormatCurrency(value, precision);

      default:
        return FormatLong(value, precision);
    }
  }

  public static String Format(
      AbbreviatedNumberForm form,
      double value)
  {
    return Format(form, value, DEFAULT_PRECISION);
  }

  public static String FormatLong(
    double value, 
    int precision)
  {
    return Format(value, AbbreviatedNumberForm.Long, precision, GetData().Long);
  }

  public static String FormatLong(double value)
  {
    return FormatLong(value, DEFAULT_PRECISION);
  }

  public static String FormatShort(
    double value, 
    int precision)
  {
    return Format(value, AbbreviatedNumberForm.Short, precision, GetData().Short);
  }

  public static String FormatCurrency(
    double value, 
    int precision)
  {
    return Format(value, AbbreviatedNumberForm.Currency, precision, GetData().Currency);
  }

  public static void Register(String id, AbbreviationRule[] longRules, AbbreviationRule[] shortRules, AbbreviationRule[] currencyRules)
  {
    NumberData data;

    if (!datas.TryGetValue(id, out data))
    {
      data = new NumberData();
      datas.Add(id, data);
    }

    data.Long = Sort(longRules);
    data.Short = Sort(shortRules);
    data.Currency = Sort(currencyRules);
  }

  private static AbbreviationRule[] Sort(AbbreviationRule[] rules)
  {
    Array.Sort<AbbreviationRule>(rules, new ReverseComparer().Compare);
    return rules;
  }

  private static int GetValuePrecision(double value)
  {
    int result = 0;
    ulong valueRange = 1;

    while (valueRange <= value)
    {
      valueRange *= 10;
      result++;
    }

    return result;
  }

  private static double RoundToSignificantDigits(double value, int digits)
  {
    if (value == 0)
      return value;

    var scale = Math.Pow(10, Math.Floor(Math.Log10(Math.Abs(value))) + 1);
    return scale * Math.Round(value / scale, digits);
  }

  private static String DeleteTrailingZeros(String value)
  {
    if (value.IndexOf(NumberFormatInfo.CurrentInfo.NumberDecimalSeparator) == -1)
      return value;

    var result = value;

    var i = result.Length - 1;

    while ((i > 0) && !((result[i].ToString() == NumberFormatInfo.CurrentInfo.NumberDecimalSeparator) || ((result[i] >= '0') && (result[i] <= '9'))))
      i--;

    while ((i >= 0) && (result[i] == '0'))
    {
      result = result.Remove(i, 1);
      i--;
    }

    if ((i >= 0) && (result[i].ToString() == NumberFormatInfo.CurrentInfo.NumberDecimalSeparator))
      result = result.Remove(i, 1);

    return result;
  }

  private static String GetStr(double value, int precision)
  {
    var valuePrecision = GetValuePrecision(value);
    value = RoundToSignificantDigits(value, precision);
    return DeleteTrailingZeros(value.ToString("F" + Math.Max(0, precision - valuePrecision)));
  }

  private static String GetNotAbbreviatedStr(double value, AbbreviatedNumberForm form, int precision)
  {
    if (form == AbbreviatedNumberForm.Currency)
    {
      var valuePrecision = GetValuePrecision(value);
      value = RoundToSignificantDigits(value, precision);
      string result;

      if (value == Math.Round(value))
        result = value.ToString("C0");
      else
        result = value.ToString("C" + Math.Max(0, precision - valuePrecision));

      return DeleteTrailingZeros(result);
    }
    else
      return GetStr(value, precision);
  }

  private static AbbreviationRule Find(ulong range, PluralForm plural, AbbreviationRule[] rules)
  {
    foreach (var rule in rules)
      if ((rule.Range == range) && (rule.Plural == plural))
        return rule;

    return null;
  }

  private static String Format(double value, AbbreviatedNumberForm form, int precision, AbbreviationRule[] rules)
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

        String str;
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

        int count = (int)Math. .Round(displayValue);

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
              result = newRule.Value;
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

  private static NumberData GetData()
  {
    AbbreviatedNumberData.Initialize();

    var id = CultureInfo.CurrentUICulture.ToString();

    if (!datas.ContainsKey(id))
    {
      id = CultureInfo.CurrentUICulture.TwoLetterISOLanguageName;

      if (!datas.ContainsKey(id))
        id = "";
    }

    return datas[id];
  }

  private static readonly AbbreviationRule[] NULL_RULES = new AbbreviationRule[0];

  static AbbreviatedNumber()
  {
    Register("", NULL_RULES, NULL_RULES, NULL_RULES);
  }
}
