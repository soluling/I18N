using System;
using System.Collections.Generic;
using System.Globalization;
using System.Resources;

namespace Soluling
{
  /// <summary>
  /// Enumeration that specifies the plural form. There are six different plural forms from singular to various plurals. 
  /// How many forms a language uses depends on the language. Most languages only use singular and plural.
  /// </summary>
  public enum Plural
  {
    ///<summary>Nullar. Used when count is zero. Few languages support this but you can include a =0 pattern into your pattern string if you want to handle 0 in a different way.</summary>
    Zero,
    ///<summary>Singular. Used when count is one. Most languages support this.</summary>
    One,
    ///<summary>Dual. Used when count is two. Few languages support this.</summary>
    Two,
    ///<summary>Trial, paucal, sexal, minority plural or plural-100. Used when count is few. Few languages support this. The range depends on the language. Most often this is something between 2 and 4.</summary>
    Few,
    ///<summary>Greater paucal. Used when count is many. Few languages support this. The range depends on the language. Most often this is more than 4.</summary>
    Many,
    ///<summary>Plural or universal. Used when count does not belong to any other group. All languages support this. Most often this is the plural form.</summary>
    Other
  };
  
  /// <summary>
  /// Enumeration that specifies the gender form. There are three different gender forms. 
  /// How many forms a language uses depends on the language. Many languages do not use gender so they only use other.
  /// </summary>
  public enum Gender
  {
    ///<summary>Used with male words.</summary>
    Male,
    ///<summary>Used with femlate words.</summary>
    Female,
    ///<summary>Other or neutral. Used when language does not use gender or gender is neutral. Most languages support this.</summary>
    Neutral
  };

  /// <summary>
  /// Enumeration that specifies the operator.
  /// </summary>
  public enum OperatorKind
  {
    /// <summary>Equal (e.g. =1).</summary>
    Equal,
    /// <summary>Around (e.g. ~1).</summary>
    Around,
    /// <summary>Less than (e.g. &lt;1).</summary>
    Less,
    /// <summary>Less or equal than (e.g. &lt;=1).</summary>
    LessOrEqual,
    /// <summary>Greater than (e.g. &gt;1).</summary>
    Greater,
    /// <summary>Greater or equal than (e.g. &gt;=1).</summary>
    GreaterOrEqual,
    /// <summary>Range (e.g 3..5).</summary>
    Range
  };

  /// <summary>
  /// Represents the method that will get the plural form matching the given number.
  /// </summary>
  /// <param name="n">Absolute value of the source number (integer and decimals) (e.g. 9.870 => 9.87)</param>
  /// <param name="i">Integer digits of n (e.g. 9.870 => 9)</param>
  /// <param name="v">Number of visible fraction digits in n, with trailing zeros (e.g. 9.870 => 3)</param>
  /// <param name="w">Number of visible fraction digits in n, without trailing zeros (e.g. 9.870 => 2)</param>
  /// <param name="f">Visible fractional digits in n, with trailing zeros (e.g. 9.870 => 870)</param>
  /// <param name="t">Visible fractional digits in n, without trailing zeros (e.g. 9.870 => 87)</param>
  /// <returns>Plural form function</returns>
  public delegate Plural PluralProc(decimal n, int i, int v, int w, int f, int t);

  /// <summary>
  /// Class that contain one pattern.
  /// </summary>
  public class Pattern
  {
    /// <summary>Pattern string.</summary>
    public string Value { get; set; }
  }

  /// <summary>
  /// Base class for plural patterns.
  /// </summary>
  public class NumberPattern: Pattern
  {
  }

  /// <summary>
  /// Class that contain one plural pattern.
  /// </summary>
  public class PluralPattern: NumberPattern
  {
    /// <summary>Plural form of the pattern.</summary>
    public Plural Plural { get; set; }

    /// <summary>
    /// Initializes a new instance of the PluralPattern class based on the specified pattern and plural type.
    /// </summary>
    /// <param name="value">Strings pattern that is used when below plural form.</param>
    /// <param name="plural">Plural form that is used.</param>
    public PluralPattern(string value, Plural plural)
    {
      Value = value;
      Plural = plural;
    }
  }

  /// <summary>
  /// Class that contain one operator pattern.
  /// </summary>
  public class OperatorPattern: NumberPattern
  {
    /// <summary>Operator value of the pattern.</summary>
    public OperatorKind Kind { get; set; }

    /// <summary>Operand value of the pattern.</summary>
    public int Operand { get; set; }

    /// <summary>Second operand value of the pattern.</summary>
    public int Operand2 { get; set; }

    /// <summary>
    /// Initializes a new instance of the OperatorPattern class based on the specified pattern and operator value.
    /// </summary>
    /// <param name="value">Strings pattern that is used when below operator value.</param>
    /// <param name="kind">Operator kind.</param>
    /// <param name="operand">Operator operand.</param>
    /// <param name="operand2">Second operator operand.</param>
    public OperatorPattern(string value, OperatorKind kind, int operand, int operand2 = 0)
    {
      Value = value;
      Kind = kind;
      Operand = operand;
      Operand2 = operand2;
    }
  }

  /// <summary>
  /// Class that contain one gender pattern.
  /// </summary>
  public class GenderPattern: Pattern
  {
    /// <summary>Gender of the pattern.</summary>
    public Gender Gender { get; set; }

    /// <summary>
    /// Initializes a new instance of the GenderPattern class based on the specified pattern and gender.
    /// </summary>
    /// <param name="value">Strings pattern that is used when below plural form.</param>
    /// <param name="gender">Gender that is used.</param>
    public GenderPattern(string value, Gender gender)
    {
      Value = value;
      Gender = gender;
    }
  }

  /// <summary>
  /// Class that contain one select pattern.
  /// </summary>
  public class SelectPattern: Pattern
  {
    /// <summary>Select value of the pattern.</summary>
    public string Select { get; set; }

    /// <summary>
    /// Initializes a new instance of the SelectPattern class based on the specified pattern and select value.
    /// </summary>
    /// <param name="value">Strings pattern that is used when below select value.</param>
    /// <param name="select">Select value that is used.</param>
    public SelectPattern(string value, string select)
    {
      Value = value;
      Select = select;
    }
  }

  /// <summary>
  /// Class that contains one part of a plural and gender enabed format string.
  /// </summary>
  public class FormatPart
  {
    private List<Pattern> items = new List<Pattern>();

    /// <summary>
    /// 
    /// </summary>
    public string Name { get; set; }

    /// <summary>
    /// 
    /// </summary>
    public string ParameterType { get; set; }

    /// <summary>
    /// Get pattern enumertor.
    /// </summary>
    /// <returns>Enumerator</returns>
    public IEnumerator<Pattern> GetEnumerator()
    {
      return items.GetEnumerator();
    }

    /// <summary>Gets the number of patterns contained.</summary>
    public int Count 
    { 
      get { return items.Count; } 
    }

    /// <summary>
    /// Gets the pattern at the specified index.
    /// </summary>
    /// <param name="index">The zero-based index of the pattern to get.</param>
    /// <returns>The pattern at the specified index.</returns>
    public Pattern this[int index]
    {
      get { return items[index]; }
    }

    /// <summary>
    /// Gets the pattern for the specified plural form.
    /// </summary>
    /// <param name="plural">The plural form of the pattern to get</param>
    /// <returns>The pattern for the specified plural form.</returns>
    public string this[Plural plural]
    {
      get 
      {
        var item = Find(plural);

        if (item != null)
          return item.Value;
        else
          return "";
      }

      set
      {
        var item = Find(plural);

        if (item != null)
          item.Value = value;
        else
          Add(value, plural);
      }
    }

    /// <summary>
    /// Gets the pattern for the specified gender.
    /// </summary>
    /// <param name="gender">The gender of the pattern to get</param>
    /// <returns>The pattern for the specified gender.</returns>
    public string this[Gender gender]
    {
      get 
      {
        var item = Find(gender);

        if (item != null)
          return item.Value;
        else
          return "";
      }

      set
      {
        var item = Find(gender);

        if (item != null)
          item.Value = value;
        else
          Add(value, gender);
      }
    }

    /// <summary>
    /// Gets the pattern for the specified select value.
    /// </summary>
    /// <param name="select">The gender of the pattern to get</param>
    /// <returns>The pattern for the specified select value.</returns>
    public string this[string select]
    {
      get 
      {
        var item = Find(select);

        if (item != null)
          return item.Value;
        else
          return "";
      }

      set
      {
        var item = Find(select);

        if (item != null)
          item.Value = value;
        else
          Add(value, select);
      }
    }

    /// <summary>
    /// Check if the part is a number part such as plural or operator.
    /// </summary>
    public bool IsNumber
    { 
      get
      {
        for (var i = 0; i < Count; i++)
          if (this[i] is NumberPattern)
            return true;

        return false;
      }
    }

    /// <summary>
    /// Check if the part is a gender part.
    /// </summary>
    public bool IsGender
    { 
      get
      {
        for (var i = 0; i < Count; i++)
          if (this[i] is GenderPattern)
            return true;

        return false;
      }
    }

    /// <summary>
    /// Check if the part is a select part.
    /// </summary>
    public bool IsSelect
    {
      get
      {
        for (var i = 0; i < Count; i++)
          if (this[i] is SelectPattern)
            return true;

        return false;
      }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    public OperatorPattern FindOperator(uint value)
    {
      OperatorPattern result = null;

      // Equal: Find first match
      for (var i = 0; i < Count; i++)
      {
        var pattern = this[i];

        if (pattern is OperatorPattern)
        {
          result = (OperatorPattern)pattern;

          if ((result.Kind == OperatorKind.Equal) && (value == result.Operand))
            return result;
        }
      }

      // Around: Find first match
      for (var i = 0; i < Count; i++)
      {
        var pattern = this[i];

        if (pattern is OperatorPattern)
        {
          result = (OperatorPattern)pattern;

          if ((result.Kind == OperatorKind.Around) && (value >= result.Operand - MultiPattern.OperatorDelta) && (value <= result.Operand + MultiPattern.OperatorDelta))
            return result;
        }
      }

      // Range: Find first match
      for (var i = 0; i < Count; i++)
      {
        var pattern = this[i];

        if (pattern is OperatorPattern)
        {
          result = (OperatorPattern)pattern;

          if ((result.Kind == OperatorKind.Range) && (value >= result.Operand) && (value <= result.Operand2))
            return result;
        }
      }

      // <, <=, > or >=: Find match that is closes to the value
      result = null;

      for (var i = 0; i < Count; i++)
      {
        var pattern = this[i];

        if (pattern is OperatorPattern)
        {
          var thisPattern = (OperatorPattern)pattern;

          if (
              ((thisPattern.Kind == OperatorKind.LessOrEqual) && (value <= thisPattern.Operand) ||
              ((thisPattern.Kind == OperatorKind.Less) && (value < thisPattern.Operand)) ||
              ((thisPattern.Kind == OperatorKind.GreaterOrEqual) && (value >= thisPattern.Operand) ||
              ((thisPattern.Kind == OperatorKind.Greater) && (value > thisPattern.Operand))
             ) &&
            ((result == null) || (Math.Abs(value - thisPattern.Operand) < Math.Abs(value - result.Operand)))))
          {
            result = thisPattern;
          }
        }
      }

      return result;
    }

    /// <summary>
    /// Check if a matching pattern exists.
    /// </summary>
    /// <param name="count">Number to be checked.</param>
    /// <param name="plural">Return the plural form.</param>
    /// <returns>true if a pattern exists.</returns>
    public bool ExistsMatching(uint count, out Plural plural)
    {
      var pattern = FindMatching(count);

      if (pattern != null)
        plural = pattern.Plural;
      else
        plural = Plural.Other;

      return pattern != null;
    }

    /// <summary>
    /// Check if a matching pattern exists.
    /// </summary>
    /// <param name="count">Number to be checked.</param>
    /// <returns>Plural form.</returns>
    public PluralPattern FindMatching(uint count)
    {
      for (var i = 0; i < Count; i++)
      {
        var pattern = this[i];

        if (pattern is PluralPattern)
        {
          var result = (PluralPattern)pattern;

          if (((count == 0) && (result.Plural == Plural.Zero)) ||
            ((count == 1) && (result.Plural == Plural.One)) ||
            ((count == 2) && (result.Plural == Plural.Two)))
          {
            return result;
          }
        }
      }

      return null;
    }

    /// <summary>
    /// Get a plural pattern for given plural form and number.
    /// </summary>
    /// <param name="plural">Plural form.</param>
    /// <param name="count">Number that is going to use the pattern.</param>
    /// <returns>Pattern.</returns>
    public string GetPluralPattern(Plural plural, uint count)
    {
      if (!IsNumber)
      {
        if (MultiPattern.RaiseExceptionOnInvalidPattern)
          throw new Exception("Pattern is not a number pattern");
        else
          return "";
      }

      string value;

      var operatorPattern = FindOperator(count);

      if (operatorPattern != null)
      {
        value = operatorPattern.Value;

        if (value != "")
          return value;
      }

      Plural matchingPlural;

      if (ExistsMatching(count, out matchingPlural))
      {
        value = this[matchingPlural];

        if (value != "")
          return value;
      }

      value = this[plural];

      if (value != "")
        return value;

      return FirstValue;
    }

    /// <summary>
    /// Get a gender pattern for given gender.
    /// </summary>
    /// <param name="gender">Gender value</param>
    /// <returns>Pattern.</returns>
    public string GetGenderPattern(Gender gender)
    {
      if (!IsGender)
        throw new Exception("Pattern is not a gender pattern");

      string value = this[gender];

      if (value == "")
        value = this.FirstValue;

      return value;
    }

    /// <summary>
    /// Gets the first pattern.
    /// </summary>
    public string FirstValue
    {
      get 
      { 
        if (Count > 0)
          return this[0].Value; 
        else
          return "";
      }
    }

    /// <summary>
    /// Gets the pattern for other plural form.
    /// </summary>
    public string OtherValue
    {
      get { return this[Plural.Other]; }
    }

    /// <summary>
    /// Gets the default pattern.
    /// </summary>
    public string DefaultValue
    {
      get
      {
        if (IsNumber)
          return this[Plural.Other];
        else
        {
          var result = this[Gender.Neutral];

          if (result == "")
            result = this[MultiPattern.OTHER];

          if (result == "")
            result = this[MultiPattern.NEUTRAL];

          if (result == "")
            result = FirstValue;

          return result;
        }
      }
    }

    /// <summary>
    /// Searches for an pattern for the plural form, and returns it.
    /// </summary>
    /// <param name="plural">The plural form of the pattern to get.</param>
    /// <returns>The pattern for the specified plural form.</returns>
    public PluralPattern Find(Plural plural)
    {
      foreach (var pattern in items)
        if ((pattern is PluralPattern) && (((PluralPattern)pattern).Plural == plural))
          return (PluralPattern)pattern;

      return null;
    }

    /// <summary>
    /// Searches for an pattern for the gender, and returns it.
    /// </summary>
    /// <param name="gender">The gende of the pattern to get.</param>
    /// <returns>The pattern for the specified gender.</returns>
    public GenderPattern Find(Gender gender)
    {
      foreach (var pattern in items)
        if ((pattern is GenderPattern) && (((GenderPattern)pattern).Gender == gender))
          return (GenderPattern)pattern;

      return null;
    }

    /// <summary>
    /// Find gender pattern.
    /// </summary>
    /// <returns></returns>
    public GenderPattern FindGender()
    {
      foreach (var pattern in items)
        if (pattern is GenderPattern)
          return (GenderPattern)pattern;

      return null;
    }

    /// <summary>
    /// Searches for an pattern for the gender, and returns it.
    /// </summary>
    /// <param name="select">The select vaöue of the pattern to get.</param>
    /// <returns>The pattern for the specified gender.</returns>
    public SelectPattern Find(string select)
    {
      foreach (var pattern in items)
        if ((pattern is SelectPattern) && (((SelectPattern)pattern).Select == select))
          return (SelectPattern)pattern;

      return null;
    }

    /// <summary>
    /// Determines whether the PluralParameter contains pattern for a plural form.
    /// </summary>
    /// <param name="plural">The plural form of the pattern to check.</param>
    /// <returns>true if the PluralParameter contains a pattern for the plural form; otherwise, false.</returns>
    public bool Exists(Plural plural)
    {
      return Find(plural) != null;
    }

    /// <summary>
    /// Determines whether the PluralParameter contains pattern for a gender.
    /// </summary>
    /// <param name="gender">The gender of the pattern to check.</param>
    /// <returns>true if the PluralParameter contains a pattern for the gender; otherwise, false.</returns>
    public bool Exists(Gender gender)
    {
      return Find(gender) != null;
    }

    /// <summary>
    /// Adds a plural pattern to the PluralParameter.
    /// </summary>
    /// <param name="value">The pattern string.</param>
    /// <param name="plural">The plural form of the pattern.</param>
    /// <returns>The PluralPattern item that was added.</returns>
    public PluralPattern Add(string value, Plural plural)
    {
      var item = new PluralPattern(value, plural);
      items.Add(item);
      return item;
    }

    /// <summary>
    /// Adds an operator pattern to the PluralParameter.
    /// </summary>
    /// <param name="value">The pattern string.</param>
    /// <param name="kind">The operator kind.</param>
    /// <param name="operand">The opertor perand.</param>
    /// <param name="operand2">The second opertor perand.</param>
    /// <returns>The OperatorPattern item that was added.</returns>
    public OperatorPattern Add(string value, OperatorKind kind, int operand, int operand2 = 0)
    {
      var item = new OperatorPattern(value, kind, operand, operand2);
      items.Add(item);
      return item;
    }

    /// <summary>
    /// Adds a gender pattern to the PluralParameter.
    /// </summary>
    /// <param name="value">The pattern string.</param>
    /// <param name="gender">The gender of the pattern.</param>
    /// <returns>The GenderPattern item that was added.</returns>
    public GenderPattern Add(string value, Gender gender)
    {
      var item = new GenderPattern(value, gender);
      items.Add(item);
      return item;
    }

    /// <summary>
    /// Adds a select pattern to the PluralParameter.
    /// </summary>
    /// <param name="value">The pattern string.</param>
    /// <param name="select">The select value of the pattern.</param>
    /// <returns>The SelectPattern item that was added.</returns>
    public SelectPattern Add(string value, string select)
    {
      var item = new SelectPattern(value, select);
      items.Add(item);
      return item;
    }
  }

  /// <summary>
  /// Enumeration that specifies the pattern format.
  /// </summary>
  public enum FormatMessageSyntax
  {
    ///<summary>ICU message format.</summary>
    Icu,
    ///<summary>Soluling's legacy format.</summary>
    Legacy
  };

  /// <summary>
  /// Class that contains patterns of plural and/or gender enabled format string.
  /// </summary>
  public class FormatString
  {
    private List<FormatPart> items = new List<FormatPart>();
    private static readonly string NEXT = "next";

    /// <summary>
    /// Initializes a new instance of the PluralString class based on the specified pattern.
    /// </summary>
    /// <param name="patterns">The pattern.</param>
    public FormatString(string patterns)
    {
      StartPattern = "";
      ParsePatterns(patterns);
    }

    /// <summary>
    /// Get the syntax that the format strings uses.
    /// </summary>
    public FormatMessageSyntax Syntax { get; private set; }

    /// <summary>
    /// Get enumerator that enums the format parts.
    /// </summary>
    /// <returns></returns>
    public IEnumerator<FormatPart> GetEnumerator()
    {
      return items.GetEnumerator();
    }

    /// <summary>Optional start pattern. Used when there are more than one plural enabled parameters in a string.</summary>
    public string StartPattern { get; set; }

    /// <summary>Gets the number of parameters contained.</summary>
    public int Count
    {
      get { return items.Count; }
    }

    /// <summary>
    /// Gets the parameter at the specified index.
    /// </summary>
    /// <param name="index">The zero-based index of the parameter to get.</param>
    /// <returns>The parameter at the specified index.</returns>
    public FormatPart this[int index]
    {
      get { return items[index]; }
    }

    /// <summary>
    /// Gets the pattern for the specified plural form.
    /// </summary>
    /// <param name="form"></param>
    /// <returns>The pattern for the specified plural form.</returns>
    public string this[Plural form]
    {
      get { return items[0][form]; }
    }

    /// <summary>
    /// Gets the pattern for the specified gender.
    /// </summary>
    /// <param name="gender"></param>
    /// <returns>The pattern for the specified gender.</returns>
    public string this[Gender gender]
    {
      get { return items[0][gender]; }
    }

    /// <summary>
    /// Gets the pattern for the specified select value.
    /// </summary>
    /// <param name="select"></param>
    /// <returns>The pattern for the specified select value.</returns>
    public string this[string select]
    {
      get { return items[0][select]; }
    }

    /// <summary>
    /// Adds a new parameter to the message.
    /// </summary>
    /// <returns>The added parameter.</returns>
    public FormatPart AddParameter()
    {
      var result = new FormatPart();
      items.Add(result);
      return result;
    }

    /// <summary>
    /// 
    /// </summary>
    public void Clear()
    {
      items.Clear();
    }

    /// <summary>
    /// Searches for an pattern for the plural form, and returns it.
    /// </summary>
    /// <param name="form">The plural form.</param>
    /// <param name="all">If <b>true</b> then all parts are used.</param>
    /// <returns>The pattern found if any.</returns>
    public PluralPattern Find(Plural form, bool all = true)
    {
      PluralPattern result;

      if (all)
      {
        for (int i = 0; i < Count; i++)
        {
          result = this[i].Find(form);

          if (result != null)
            return result;
        }

        return null;
      }
      else
        return this[0].Find(form);
    }

    /// <summary>
    /// Checks if the string contains a plural form.
    /// </summary>
    /// <param name="plural">Plural form to be checked</param>
    /// <returns>True if the string contains the plural form.</returns>
    public bool Exists(Plural plural)
    {
      return Find(plural) != null;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="gender"></param>
    /// <param name="all"></param>
    /// <returns></returns>
    public GenderPattern Find(Gender gender, bool all = true)
    {
      GenderPattern result;

      if (all)
      {
        for (int i = 0; i < Count; i++)
        {
          result = this[i].Find(gender);

          if (result != null)
            return result;
        }

        return null;
      }
      else
        return this[0].Find(gender);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="gender"></param>
    /// <returns></returns>
    public bool Exists(Gender gender)
    {
      return Find(gender) != null;
    }

    private void SkipWhiteSpaces(string pattern, ref int index)
    {
      while ((index < pattern.Length) && (pattern[index] == ' '))
        index++;
    }

    private string GetIcuToken(string pattern, ref int index)
    {
      var result = "";

      while (index < pattern.Length)
      {
        var c = pattern[index];

        if ((c == ' ') || (c == ','))
        {
          index++;
          break;
        }

        if (c == '{')
          break;

        result = result + c;
        index++;
      }

      SkipWhiteSpaces(pattern, ref index);

      return result;
    }

    private string GetIcuPart(string pattern, ref int index)
    {
      index++;
      var braces = 1;
      var result = "";

      while ((index < pattern.Length) && (braces > 0))
      {
        var c = pattern[index];

        if (c == '{')
          braces++;
        else if (c == '}')
          braces--;

        if (braces == 0)
          break;

        if (c == '\\')
        {
          index++;
          c = pattern[index];
        }

        result += c; 
        index++;
      }

      index++;
      SkipWhiteSpaces(pattern, ref index);

      return result;
    }

    private bool ParseIcuPattern(string pattern, ref int index)
    {
      // {file, plural one {{0} file} other {{0} files}}
      if (pattern[index] != '{')
        return false;

      var parameter = AddParameter();

      index++;
      parameter.Name = GetIcuToken(pattern, ref index);
      parameter.ParameterType = GetIcuToken(pattern, ref index);

      while (index < pattern.Length)
      {
        if (pattern[index] == '}')
          break;

        string name;

        if (pattern[index] == '{')
        {
          name = parameter.ParameterType;
          parameter.ParameterType = parameter.Name;
          parameter.Name = "";
        }
        else
          name = GetIcuToken(pattern, ref index);

        var str = GetIcuPart(pattern, ref index);

        if (parameter.ParameterType == "plural")
        {
          if (MultiPattern.IsPluralForm(name))
            parameter.Add(str, MultiPattern.StringToPlural(name));
          else
          {
            OperatorKind operatorKind;
            int operand;
            int operand2;

            if (MultiPattern.StringToOperator(name, out operatorKind, out operand,  out operand2))
              parameter.Add(str, operatorKind, operand, operand2);
          }
        }
        else if (parameter.ParameterType == "gender")
          parameter.Add(str, MultiPattern.StringToGender(name));
        else
          parameter.Add(str, name);
      }

      index++;
      return true;
    }

    private bool IsIcuPattern(string pattern)
    {
      if ((pattern.IndexOf('{') == -1) || (pattern.IndexOf('}') == -1))
        return false;

      var level = 0;
      var result = false;

      for (var j = 0; j < pattern.Length; j++)
      {
        var c = pattern[j];

        if ((c == '{') && ((j == 0) || (pattern[j] != '\\')))
          level++;
        else if (c == '}')
          level--;

        if (level >= 2)
        {
          result = true;
          break;
        }
      }

      return result;
    }

    private bool IsLegacyPattern(string pattern)
    {
      if ((pattern.IndexOf("other;") >= 0) || (pattern.IndexOf("neutral;") >= 0))
        return true;

      var count = 0;

      foreach (var c in pattern)
        if (c == ';')
        {
          count++;

          if (count >= 2)
            return true;
        }

      return false;
    }

    private bool ParseIcu(string patterns)
    {
      if (!IsIcuPattern(patterns))
        return false;

      // {file, plural one {{0} file} other {{0} files}}
      // I have {file, plural one {{0} file} other {{0} files}.}
      var startPattern = "";
      var placeholder = "";
      var placeholderIndex = 0;
      var i = 0;

      while (i < patterns.Length)
      {
        var c = patterns[i];

        if (c == '{')
        {
          ParseIcuPattern(patterns, ref i);

          placeholder = "{" + placeholderIndex.ToString() + "}";
          placeholderIndex++;

          if (startPattern != "")
          {
            startPattern += placeholder;
            placeholder = "";
          }

          continue;
        }
        else if (c == '\\')
        {
          i++;
          c = patterns[i];
          startPattern = startPattern + c;
        }
        else
        {
          if (placeholder != "")
          {
            startPattern += placeholder;
            placeholder = "";
          }

          startPattern = startPattern + c;
        }

        i++;
      }

      StartPattern = startPattern;
      Syntax = FormatMessageSyntax.Icu;

      return true;
    }

    private bool ParseLegacy(string patterns)
    {
      var pluralStr = "";
      var operatorStr = "";
      var genderStr = "";
      var selectStr = "";
      var parameter = AddParameter();
      var i = 0;

      while (i < patterns.Length)
      {
        var str = "";

        while (i < patterns.Length)
        {
          var c = patterns[i];

          if (c == ';')
          {
            i++;

            if ((i >= patterns.Length) || (patterns[i] != ';'))
              break;
          }

          str = str + c;
          i++;
        }

        if (str == NEXT)
        {
          // New parameter
          parameter = AddParameter();
        }
        else if (pluralStr != "")
        {
          // Add plural pattern
          parameter.Add(str, MultiPattern.StringToPlural(pluralStr));
          pluralStr = "";
        }
        else if (operatorStr != "")
        {
          // Add plural pattern
          OperatorKind kind;
          int operand = MultiPattern.StringToOperator(operatorStr, out kind);
          parameter.Add(str, kind, operand);
          operatorStr = "";
        }
        else if (genderStr != "")
        {
          // Add gender pattern
          parameter.Add(str, MultiPattern.StringToGender(genderStr));
          genderStr = "";
        }
        else if (selectStr != "")
        {
          // Add selectStr pattern
          parameter.Add(str, selectStr);
          selectStr = "";
        }
        else if ((selectStr == "") && (Count == 1) && (str.IndexOf('{') >= 0) && (StartPattern != ""))
        {
          selectStr = StartPattern;
          StartPattern = "";
          parameter.Add(selectStr, str);
          selectStr = "";
        }
        else if (str == MultiPattern.OTHER)
        {
          if (parameter.FindGender() != null)
            genderStr = str;
          else
            pluralStr = str;
        }
        else if (MultiPattern.IsGender(str))
        {
          // Gender pattern follows
          genderStr = str;
        }
        else if (MultiPattern.IsPluralForm(str))
        {
          // Plural pattern follows
          pluralStr = str;
        }
        else if (MultiPattern.IsOperator(str))
        {
          // Operator pattern follows
          operatorStr = str;
        }
        else if ((StartPattern != "") || (parameter.Count > 0))
          selectStr = str;
        else 
          StartPattern = str;
      }

      Syntax = FormatMessageSyntax.Legacy;
      return true;
    }

    private void ParsePatterns(string patterns)
    {
      if (!ParseIcu(patterns))
      {
        // Not an ICU message. Try the legacy format
        Clear();

        if (!ParseLegacy(patterns))
          throw new Exception(String.Format("\"{0}\" is not a valid patterns strings", patterns));
      }
    }
  }

  /// <summary>
  /// Implements a plural, gender and select enabled string format functions.
  /// </summary>
  /// <remarks>
  /// Most languages have only single and plural forms. However this is not case with every language. Asian languages do not have plural form at all. 
  /// They only have single form. Most Slavic languages have three forms: singular, plural and one between them (dual or paucal). 
  /// Sometimes zero is handled as plural, sometimes as singular. Some languages have four different forms. Arabic has six different forms.
  /// 
  /// This is why you can not use the standard Format if you want to create grammatically correct dynamic messages that contain counts. Instead you can
  /// use Plural.Format function. It works little bit like GetText's ngettext funtion. You pass combined pattern and count parameter. The functions
  /// calculates what pattern to use based on the passed count parameter and returns that pattern.
  /// 
  /// Plural.Format works in the same way as String.Format but instead of having only one pattern in a string you will have one or more depending on 
  /// the grammatical rules of the language. English for example uses singular and plural. it is very common to write something line this
  /// <code>str = String.Format("{0} file(s)", fileCount);</code>
  /// This is very common but not grammatically correct. The right way would to use either "{0} file" or "{0} files" depending on the actual file count.
  /// Using this class you can include all different pattern into one string by using the syntax the string contains sub parts for each grammatical rule.
  /// The English string would be "{files, plural, one {{0} file} other {{0} files}}" that contains two parts: one and other. One is for singular form and other is for plural form.
  /// <code>str = Plural.Format("{fileCount, plural, one {{0} file} other {{0} files}}", fileCount);</code>
  /// You can also use the legacy format:
  /// <code>str = Plural.Format("one;{0} file;other;{0} files", fileCount);</code>
  /// If you pattern contains {, } or \ characters you must escape them
  /// <code>"Item {\} {0} car(s)"</code>
  /// becomes to 
  /// <code>"{cars, plural, one {Item \{\\\} {0} car} other {Item \{\\\} {0} cars}}".</code>
  /// <para>
  /// Remember to end the resource string item name with Plural such as MyStringPlural. That way Soluling can identify the pattern as multi pattern string and 
  /// it can enable advanced plural translation editor.
  /// </para>
  /// <para>
  /// Plural engine need to know the language that the application uses. Compiled application file does not contain this information. 
  /// This is why you should create a resource string that contains the locale id and set that value to <see cref="Language.Id"/>. 
  /// A good place to set it is the constructor of your application.
  /// </para>
  /// </remarks>
  /// <example>
  /// Following example show how to create grammatically correct "{0} file(s)" message. 
  /// English uses singular and plural so we need one and other parts in the pattern.
  /// <code>
  /// int count;
  /// ...
  /// textLabel.Text = Plural.Format("{files, plural, one {{0} file} other {{0} files}}", count);
  /// </code>
  /// In a real application you would not hard code the pattern strings into the code but use resource strings.
  /// <code>
  /// int count;
  /// ...
  /// textLabel.Text = Plural.Format(Properties.Resources.MessageStringPlural, count);
  /// </code>
  /// If you want to handle 0 in a different way you can add zero part into any pattern on any language even if the language does not use different grammar in the case of zero.
  /// <code>
  /// int count;
  /// ...
  /// textLabel.Text = Plural.Format("{files, plural, zero {No files} one {{0} file} other {{0} files}}", count);
  /// </code>
  /// </example>
  public static class MultiPattern
  {
    private static Dictionary<string, PluralProc> procs = new Dictionary<string, PluralProc>();

    /// <summary>
    /// 
    /// </summary>
    public static bool RaiseExceptionOnInvalidPattern { get; set; }

    /// <summary>
    /// 
    /// </summary>
    public static int OperatorDelta { get; set; }

    internal const string OTHER = "other";
    internal const string NEUTRAL = "neutral";

    private static readonly string[] PLURAL_DATAS = new string[]
    {
      "zero",
      "one",
      "two",
      "few",
      "many",
      OTHER
    };

    private static readonly string[] GENDER_DATAS = new string[]
    {
      "male",
      "female",
      OTHER
    };

    /// <summary>
    /// 
    /// </summary>
    /// <param name="id"></param>
    /// <param name="proc"></param>
    public static void Register(string id, PluralProc proc)
    {
      procs.Add(id, proc);
    }

    private static bool IsLanguageInArray(string language, string[] array)
    {
      foreach (var thisLanguage in array)
        if (thisLanguage == language)
          return true;

      return false;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    public static bool IsSingleFormLanguage()
    {
      return GetProc() == PluralData.GetSinglePlural;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="id"></param>
    /// <returns></returns>
    public static bool IsZeroLikeOne(string id = "")
    {
      return Proc(0) == Plural.One;
    }

    /// <summary>
    /// Gets the index procedure of a specific culture.
    /// </summary>
    /// <param name="culture">Culture to be processed.</param>
    /// <returns>Plural index procedure</returns>
    public static PluralProc GetProc(CultureInfo culture)
    {
      PluralProc proc;

      if (procs.TryGetValue(culture.ToString(), out proc))
        return proc;
      else
      {
        if (procs.TryGetValue(culture.TwoLetterISOLanguageName, out proc))
          return proc;
        else
          return procs[""];
      }
    }

    /// <summary>
    /// Gets the index procedure of the current culture.
    /// </summary>
    /// <returns>Plural index procedure.</returns>
    public static PluralProc GetProc()
    {
      return GetProc(Language.Culture);
    }

    private static Plural Proc(uint count)
    {
      return GetProc()(count, (int)count, 0, 0, 0, 0);
    }

    private static string InternalGetPattern(String format, uint count)
    {
      return new FormatString(format)[0].GetPluralPattern(Proc(count), count);
    }

    private static string InternalGetPattern(String format, uint count, out string startPattern)
    {
      var str = new FormatString(format);
      startPattern = str.StartPattern;
      return str[0].GetPluralPattern(Proc(count), count);
    }

    /// <summary>
    /// Gets the part from plural enabled composite format string.
    /// </summary>
    /// <param name="manager">Resource manager that is used to find resources.</param>
    /// <param name="name">Name of the resource containing the pattern string.</param>
    /// <param name="count">The number to format.</param>
    /// <returns>A copy of format string matching the plural form of specified number.</returns>
    public static string GetPattern(ResourceManager manager, String name, uint count)
    {
      var resourceCulture = Language.GetResourceCulture(manager, name);

      if ((resourceCulture == null) || (resourceCulture.Name == ""))
        resourceCulture = Language.OriginalCulture;

      return InternalGetPattern(manager.GetString(name), (uint)count);
    }

    /// <summary>
    /// Gets the part from plural enabled composite format string.
    /// </summary>
    /// <param name="format">A plural enabled composite format string.</param>
    /// <param name="count">The number to format.</param>
    /// <returns>A copy of format string matching the plural form of specified number.</returns>
    public static string GetPattern(string format, uint count)
    {
      return InternalGetPattern(format, count);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="format"></param>
    /// <param name="count"></param>
    /// <param name="startPattern"></param>
    /// <returns></returns>
    public static string GetPattern(string format, uint count, out string startPattern)
    {
      return InternalGetPattern(format, count, out startPattern);
    }

    /// <summary>
    /// Gets the part from gender enabled composite format string.
    /// </summary>
    /// <param name="format"></param>
    /// <param name="gender"></param>
    /// <returns></returns>
    public static string GetPattern(string format, Gender gender)
    {
      return new FormatString(format)[gender];
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="format"></param>
    /// <param name="gender"></param>
    /// <param name="startPattern"></param>
    /// <returns></returns>
    public static string GetPattern(string format, Gender gender, out string startPattern)
    {
      var str = new FormatString(format);
      startPattern = str.StartPattern;
      var result = str[gender];

      if ((result == "") && (gender != Gender.Neutral))
        result = str[Gender.Neutral];

      return result;
    }

    /// <summary>
    /// Gets the part from select enabled composite format string.
    /// </summary>
    /// <param name="format"></param>
    /// <param name="select"></param>
    /// <returns></returns>
    public static string GetPattern(string format, string select)
    {
      return new FormatString(format)[select];
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="format"></param>
    /// <param name="select"></param>
    /// <param name="startPattern"></param>
    /// <returns></returns>
    public static string GetPattern(string format, string select, out string startPattern)
    {
      var str = new FormatString(format);
      startPattern = str.StartPattern;
      return str[select];
    }

    /// <summary>
    /// Convert string plural form into Plural enum.
    /// </summary>
    /// <param name="value">String name of the plural form such as "one".</param>
    /// <param name="plural">Returns the plural form.</param>
    /// <returns>true if the string value could be converted into plural form.</returns>
    public static bool StringToPlural(string value, out Plural plural)
    {
      foreach (var i in (Plural[])Enum.GetValues(typeof(Plural)))
      {
        if (String.Compare(PLURAL_DATAS[(int)i], value) == 0)
        {
          plural = i;
          return true;
        }
      }

      plural = Plural.Other;
      return false;
    }

    /// <summary>
    /// Convert string operator form into operator value.
    /// </summary>
    /// <param name="value">String name of the operator value such as "=1".</param>
    /// <param name="kind">Returns the operator kind.</param>
    /// <param name="operand">Returns the operand value.</param>
    /// <param name="operand2">Returns the operand value.</param>
    /// <returns>true if the string value could be converted into operator value.</returns>
    public static bool StringToOperator(string value, out OperatorKind kind, out int operand, out int operand2)
    {
      if (TryStringToOperator(value, out kind, out operand, out operand2))
        return true;
      else
      {
        kind = OperatorKind.Equal;
        operand = 0;
        operand2 = 0;
        return false;
      }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="value"></param>
    /// <param name="kind"></param>
    /// <param name="operand"></param>
    /// <param name="operand2"></param>
    /// <returns></returns>
    public static bool TryStringToOperator(string value, out OperatorKind kind, out int operand, out int operand2)
    {
      kind = OperatorKind.Equal;
      operand2 = 0;

      var p = value.IndexOf("..");

      if (p > 0)
      {
        kind = OperatorKind.Range;
        return int.TryParse(value.Substring(0, p), out operand) && int.TryParse(value.Substring(p + 2), out operand2);
      }
      else if ((value.IndexOf("<=") == 0) || (value.IndexOf(">=") == 0))
      {
        // >=0, <=1, <=2, ...
        if (value.IndexOf("<=") == 0)
          kind = OperatorKind.LessOrEqual;
        else if (value.IndexOf(">=") == 0)
          kind = OperatorKind.GreaterOrEqual;

        return int.TryParse(value.Substring(2), out operand);
      }
      else if ((value != "") && ((value[0] == '=') || (value[0] == '~') || (value[0] == '<') ||(value[0] == '>')))
      {
        // =0, =1, =2, ...
        switch (value[0])
        {
          case '=':
            kind = OperatorKind.Equal;
            break;

          case '~':
            kind = OperatorKind.Around;
            break;

          case '<':
            kind = OperatorKind.Less;
            break;

          case '>':
            kind = OperatorKind.Greater;
            break;
        }

        return int.TryParse(value.Substring(1), out operand);
      }
      else
      {
        operand = 0;
        return false;
      }
    }

    /// <summary>
    /// Convert string gender into Gender enum.
    /// </summary>
    /// <param name="value">String name of the gender such as "male".</param>
    /// <param name="gender">Returns the gender.</param>
    /// <returns>true if the string value could be converted into gender.</returns>
    public static bool StringToGender(string value, out Gender gender)
    {
      if (String.Compare("neutral", value) == 0)
      {
        gender = Gender.Neutral;
        return true;
      }

      foreach (var i in (Gender[])Enum.GetValues(typeof(Gender)))
      {
        if (String.Compare(GENDER_DATAS[(int)i], value) == 0)
        {
          gender = i;
          return true;
        }
      }

      gender = Gender.Neutral;
      return false;
    }

    /// <summary>
    /// Convert string plural form into Plural enum.
    /// </summary>
    /// <param name="value">String name of the plural form such as "one".</param>
    /// <returns>Plural form.</returns>
    public static Plural StringToPlural(string value)
    {
      Plural result;

      if (!StringToPlural(value, out result))
        throw new Exception(String.Format("Invalid plural form value: {0}", value));

      return result;
    }

    /// <summary>
    /// Convert string operator value into operator value.
    /// </summary>
    /// <param name="value">String name of the operator value such as "=1".</param>
    /// <param name="kind">Operator kind.</param>
    /// <returns>Operand value.</returns>
    public static int StringToOperator(string value, out OperatorKind kind)
    {
      int result, operand2;

      if (!StringToOperator(value, out kind, out result, out operand2))
        throw new Exception(String.Format("Invalid opeator value: {0}", value));

      return result;
    }

    /// <summary>
    /// Convert string gender into Gender enum.
    /// </summary>
    /// <param name="value">String name of the gender such as "male".</param>
    /// <returns>Gender</returns>
    public static Gender StringToGender(string value)
    {
      Gender result;

      if (!StringToGender(value, out result))
        throw new Exception(String.Format("Invalid gender value: {0}", value));

      return result;
    }

    /// <summary>
    /// Checks if the string value is a name of a plural form.
    /// </summary>
    /// <param name="value">String name of the plural form such as "one".</param>
    /// <returns>true if the specified string is a valid plural form.</returns>
    public static bool IsPluralForm(string value)
    {
      Plural plural;
    
      return StringToPlural(value, out plural);
    }

    /// <summary>
    /// Checks if the string value is a name of a plural form.
    /// </summary>
    /// <param name="value">String name of the plural form such as "one".</param>
    /// <returns>true if the specified string is a valid plural form.</returns>
    public static bool IsOperator(string value)
    {
      OperatorKind kind;
      int operand, operand2;
      return TryStringToOperator(value, out kind, out operand, out operand2);
    }

    /// <summary>
    /// Checks if the string value is a name of a gender.
    /// </summary>
    /// <param name="value">String name of the gender such as "male".</param>
    /// <returns>true if the specified string is a valid gender.</returns>
    public static bool IsGender(string value)
    {
      Gender gender;
    
      return StringToGender(value, out gender);
    }

    /// <summary>
    /// Like String.Format but is plural aware.
    /// </summary>
    /// <param name="patterns">A plural enabled composite format string.</param>
    /// <param name="count">The number to format.</param>
    /// <returns>A string in which the format items have been replaced by the string representation of the corresponding objects in args.</returns>
    public static string Format(string patterns, int count)
    {
      return Format(patterns, count, count);
    }

    /// <summary>
    /// Like String.Format but is plural aware.
    /// </summary>
    /// <param name="patterns">A plural enabled composite format string.</param>
    /// <param name="count">The number to format.</param>
    /// <param name="args">An object array that contains zero or more objects to format.</param>
    /// <returns>A string in which the format items have been replaced by the string representation of the corresponding objects in args.</returns>
    public static string Format(string patterns, int count, params object[] args)
    {
      string startPattern;
      var result = String.Format(GetPattern(patterns, (uint)count, out startPattern), args);

      if (!String.IsNullOrEmpty(startPattern))
        return String.Format(startPattern, result);
      else
        return result;
    }

    /// <summary>
    /// Like String.Format but is gender aware.
    /// </summary>
    /// <param name="patterns">A gender enabled composite format string.</param>
    /// <param name="gender">The gender to be used.</param>
    /// <param name="args">An object array that contains zero or more objects to format.</param>
    /// <returns>A string in which the format items have been replaced by the string representation of the corresponding objects in args.</returns>
    public static string Format(string patterns, Gender gender, params object[] args)
    {
      string startPattern;
      var result = String.Format(GetPattern(patterns, gender, out startPattern), args);

      if (!String.IsNullOrEmpty(startPattern))
        return String.Format(startPattern, result);
      else
        return result;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="patterns"></param>
    /// <param name="gender"></param>
    /// <returns></returns>
    public static Gender GetGender(string patterns, Gender gender)
    {
      var str = new FormatString(patterns);

      if (str.Exists(gender))
        return gender;
      else
      {
        var parameter = str[0];

        foreach (var pattern in parameter)
          if (pattern is GenderPattern)
            return ((GenderPattern)pattern).Gender;

        return Gender.Neutral;
      }
    }

    /// <summary>
    /// Like String.Format but is multi select aware.
    /// </summary>
    /// <param name="patterns">A gender enabled composite format string.</param>
    /// <param name="select">The select value to be used.</param>
    /// <param name="args">An object array that contains zero or more objects to format.</param>
    /// <returns>A string in which the format items have been replaced by the string representation of the corresponding objects in args.</returns>
    public static string Format(string patterns, string select, params object[] args)
    {
      string startPattern;
      var result = String.Format(GetPattern(patterns, select, out startPattern), args);

      if (!String.IsNullOrEmpty(startPattern))
        return String.Format(startPattern, result);
      else
        return result;
    }

    /// <summary>
    /// Like String.Format but is plural aware.
    /// </summary>
    /// <param name="manager">Resource manager that is used to find resources.</param>
    /// <param name="name">Name of the resource containing the pattern string.</param>
    /// <param name="count">The number to format.</param>
    /// <param name="args">An object array that contains zero or more objects to format.</param>
    /// <returns>A string in which the format items have been replaced by the string representation of the corresponding objects in args.</returns>
    public static string Format(ResourceManager manager, string name, int count, params object[] args)
    {
      return String.Format(GetPattern(manager, name, (uint)count), args);
    }

    /// <summary>
    /// Like String.Format but is plural and gender aware.
    /// </summary>
    /// <param name="patterns">A plural and/or gender enabled composite format string.</param>
    /// <param name="args">Count and gender parameters for the patterns.</param>
    /// <returns>A string in which the format items have been replaced by the string representation of the corresponding objects in args.</returns>
    public static string FormatMulti(string patterns, params object[] args)
    {
      var result = "";
      var str = new FormatString(patterns);
      var count = (uint)args.Length;

      if (count != str.Count)
        throw new Exception(String.Format("Pattern \"{0}\" does not have enough patterns for {1} parameter", patterns, count));

      if ((str.StartPattern != "") && (str.Syntax == FormatMessageSyntax.Icu))
      {
        // ICU
        var parameters = new string[str.Count];

        for (var i = 0; i < str.Count; i++)
        {
          var parameter = str[i];
          var arg = args[i];

          if (IsNumber(arg))
          {
            // Plural
            count = Convert.ToUInt32(arg);
            var plural = Proc(count);
            string pattern = parameter.GetPluralPattern(plural, count);

            parameters[i] = String.Format(pattern, arg);
          }
          else
          {
            // Gender
            var gender = Gender.Neutral;
            var value = "";
            var genderFound = false;
            var valueFound = false;
#if NETSTANDARD1_4
          var properties = arg.GetType().GetRuntimeProperties();
#else
            var properties = arg.GetType().GetProperties();
#endif

            foreach (var property in properties)
            {
              if (property.PropertyType == typeof(Gender))
              {
                gender = (Gender)property.GetValue(arg, null);
                genderFound = true;
              }
              else if (property.PropertyType == typeof(string))
              {
                value = (string)property.GetValue(arg, null);
                valueFound = true;
              }
            }

            if (!genderFound)
              throw new Exception(String.Format("No gender parameter found for: {0}", patterns));
            else if (!valueFound)
              throw new Exception(String.Format("No value parameter found for: {0}", patterns));

            var pattern = parameter[gender];

            if (pattern == "")
              pattern = parameter.FirstValue;

            parameters[i] = String.Format(pattern, value);
          }
        }

        return String.Format(str.StartPattern, parameters);
      }
      else
      {
        // Legacy
        // Process parameter from last to first and build the result string
        for (var i = str.Count - 1; i >= 0; i--)
        {
          var parameter = str[i];
          var arg = args[i];

          if (IsNumber(arg))
          {
            // Plural
            count = Convert.ToUInt32(arg);
            var plural = Proc(count);
            string pattern = parameter.GetPluralPattern(plural, count);

            if (i == str.Count - 1)
              result = String.Format(pattern, arg);
            else
              result = String.Format(pattern, arg, result);
          }
          else
          {
            // Gender
            var gender = Gender.Neutral;
            var value = "";
            var genderFound = false;
            var valueFound = false;
#if NETSTANDARD1_4
          var properties = arg.GetType().GetRuntimeProperties();
#else
            var properties = arg.GetType().GetProperties();
#endif

            foreach (var property in properties)
            {
              if (property.PropertyType == typeof(Gender))
              {
                gender = (Gender)property.GetValue(arg, null);
                genderFound = true;
              }
              else if (property.PropertyType == typeof(string))
              {
                value = (string)property.GetValue(arg, null);
                valueFound = true;
              }
            }

            if (!genderFound)
              throw new Exception(String.Format("No gender parameter found for: {0}", patterns));
            else if (!valueFound)
              throw new Exception(String.Format("No value parameter found for: {0}", patterns));

            var pattern = parameter[gender];

            if (pattern == "")
              pattern = parameter.FirstValue;

            if (i == str.Count - 1)
              result = String.Format(pattern, value);
            else
              result = String.Format(pattern, value, result);
          }
        }

        if (!String.IsNullOrEmpty(str.StartPattern))
          result = String.Format(str.StartPattern, result);

        return result;
      }
    }

    private static bool IsNumber(object value)
    {
      return 
        (value is sbyte) || 
        (value is byte) || 
        (value is short) || 
        (value is ushort) || 
        (value is int) || 
        (value is uint) || 
        (value is long) || 
        (value is ulong);
    }

    private static Plural GetDefaultPlural(decimal n, int i, int v, int w, int f, int t)
    {
      if (i == 1)
        return Plural.One;
      else
        return Plural.Other;
    }

    static MultiPattern()
    {
      OperatorDelta = 1;
      PluralData.Initialize();
      procs.Add("", GetDefaultPlural);
    }
  }
}