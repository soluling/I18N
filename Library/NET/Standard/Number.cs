using System;
using System.Collections.Generic;
using System.Globalization;

namespace Soluling
{
  /// <summary>
  /// Enumeration that specifies the format of the abbreviation type.
  /// </summary>
  public enum AbbreviatedNumberForm
  {
    /// <summary>Long string form such as "1 thousand" and "10 millions"</summary>
    Long,

    /// <summary>Short string form such as "1K" or "10M"</summary>
    Short,

    /// <summary>Currency string form such as "$1K" and "$10M"</summary>
    Currency
  }

  /// <summary>
  /// Class that stores one abbreviation rule.
  /// </summary>
  /// <remarks>
  /// Abbreviation rules use CLDR syntax where there is a set of rules for each language and abbreviation type.
  /// </remarks>
  public class AbbreviationRule
  {
    /// <summary>
    /// Minimum value where this rule is used.
    /// </summary>
    public ulong Range { get; set; }

    /// <summary>
    /// Plural form of the rule.
    /// </summary>
    public Plural Plural { get; set; }

    /// <summary>
    /// The abbreviation rule value.
    /// </summary>
    public string Value { get; set; }
  }

  class Data
  {
    public AbbreviationRule[] Long { get; set; }
    public AbbreviationRule[] Short { get; set; }
    public AbbreviationRule[] Currency { get; set; }
  }

  class ReverseComparer : IComparer<AbbreviationRule>
  {
    /// <summary>
    /// 
    /// </summary>
    /// <param name="rule1"></param>
    /// <param name="rule2"></param>
    /// <returns></returns>
    public int Compare(AbbreviationRule rule1, AbbreviationRule rule2)  
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
    }
  }

  /// <summary>
  /// Static class that contains abbreviated number routines.
  /// </summary>
  public static class AbbreviatedNumber
  {
    private static Dictionary<string, Data> datas = new Dictionary<string, Data>();

    /// <summary>
    /// Default amount of precision digits.
    /// </summary>
    public const int DEFAULT_PRECISION = 2;

    /// <summary>
    /// Converts a number to its abbreviated form.
    /// </summary>
    /// <param name="form">Abbreviation type.</param>
    /// <param name="value">Number value to be abbreviated.</param>
    /// <param name="precision">Number of digits that are used to express a value.</param>
    /// <returns>The formatted string.</returns>
    public static string Format(
      AbbreviatedNumberForm form,
      double value,
      int precision = DEFAULT_PRECISION)
    {
      switch (form)
      {
        case AbbreviatedNumberForm.Short:
          return FormatShort(value, precision);

        case AbbreviatedNumberForm.Currency:
          return FormatCurrency(value, precision);

        default:
          return FormatLong(value, precision);
      }
    }

    /// <summary>
    /// Converts a number to its abbreviated long form.
    /// </summary>
    /// <param name="value">Number value to be abbreviated.</param>
    /// <param name="precision">Number of digits that are used to express a value.</param>
    /// <returns>The formatted string.</returns>
    public static string FormatLong(
      double value, 
      int precision = DEFAULT_PRECISION)
    {
      return Format(value, AbbreviatedNumberForm.Long, precision, GetData().Long);
    }

    /// <summary>
    /// Converts a number to its abbreviated short form.
    /// </summary>
    /// <param name="value">Number value to be abbreviated.</param>
    /// <param name="precision">Number of digits that are used to express a value.</param>
    /// <returns>The formatted string.</returns>
    public static string FormatShort(
      double value, 
      int precision = DEFAULT_PRECISION)
    {
      return Format(value, AbbreviatedNumberForm.Short, precision, GetData().Short);
    }

    /// <summary>
    /// Converts a number to its abbreviated currency form.
    /// </summary>
    /// <param name="value">Number value to be abbreviated.</param>
    /// <param name="precision">Number of digits that are used to express a value.</param>
    /// <returns>The formatted string.</returns>
    public static string FormatCurrency(
      double value, 
      int precision = DEFAULT_PRECISION)
    {
      return Format(value, AbbreviatedNumberForm.Currency, precision, GetData().Currency);
    }

    /// <summary>
    /// Register formatting functions for a language.
    /// </summary>
    /// <param name="id">IETF language tag of the language/locale.</param>
    /// <param name="longRules">Rules to abbreviate a number to a long form.</param>
    /// <param name="shortRules">Rules to abbreviate a number to a short form.</param>
    /// <param name="currencyRules">Rules to abbreviate a number to a currency form.</param>
    public static void Register(string id, AbbreviationRule[] longRules, AbbreviationRule[] shortRules, AbbreviationRule[] currencyRules)
    {
#pragma warning disable IDE0018 // Inline variable declaration
      Data data;
#pragma warning restore IDE0018 // Inline variable declaration

      if (!datas.TryGetValue(id, out data))
      {
        data = new Data();
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

    private static string DeleteTrailingZeros(string value)
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

    private static string GetStr(double value, int precision)
    {
      var valuePrecision = GetValuePrecision(value);
      value = RoundToSignificantDigits(value, precision);
      return DeleteTrailingZeros(value.ToString("F" + Math.Max(0, precision - valuePrecision)));
    }

    private static string GetNotAbbreviatedStr(double value, AbbreviatedNumberForm form, int precision)
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

    private static AbbreviationRule Find(ulong range, Plural plural, AbbreviationRule[] rules)
    {
      foreach (var rule in rules)
        if ((rule.Range == range) && (rule.Plural == plural))
          return rule;

      return null;
    }

    private static string Format(double value, AbbreviatedNumberForm form, int precision, AbbreviationRule[] rules)
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
            //var proc = MultiPattern.GetIndexProc();
            var plural = MultiPattern.GetProc()(count, (int)count, 0, 0, 0, 0);

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

    private static Data GetData()
    {
      Data data;

      if (datas.TryGetValue(Language.Id, out data))
        return data;
      else
      {
        if (datas.TryGetValue(Language.Culture.TwoLetterISOLanguageName, out data))
          return data;
        else
          return datas[""];
      }
    }

    private static readonly AbbreviationRule[] NULL_RULES = new AbbreviationRule[0];

    static AbbreviatedNumber()
    {
      AbbreviatedNumberData.Initialize();
      Register("", NULL_RULES, NULL_RULES, NULL_RULES);
    }
  }
}
