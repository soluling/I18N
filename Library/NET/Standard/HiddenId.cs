using System;
using System.Collections.Generic;
using System.Text;

namespace Soluling
{
  /// <summary>
  /// 
  /// </summary>
  public enum ZeroWidthCharacter
  {
    /// <summary>200B</summary>
    Space,

    /// <summary>200C</summary>
    NonJoiner,

    /// <summary>200D</summary>
    Joiner,

    /// <summary>200E</summary>
    LeftToRightMark,

    /// <summary>202A</summary>
    LeftToRightEmbedding,

    /// <summary>202C</summary>
    PopDirectionalFormatting,

    /// <summary>202D</summary>
    LeftToRightOverwrite,

    /// <summary>2060</summary>
    WordJoiner,

    /// <summary>2062</summary>
    InvisibleTimes,

    /// <summary>2063</summary>
    InvisibleSeparator,

    /// <summary>FEFF</summary>
    NoBreakSpace
  }

  /// <summary>
  /// Class that contains one decoded string that is the string id and the string value.
  /// </summary>
  public class DecodedString
  {
    /// <summary>String id.</summary>
    public int Id { get; set; }

    /// <summary>String value.</summary>
    public string Value { get; set; }
  }

  /// <summary>
  /// Implements methods to inject a hidden id into a string.
  /// </summary>
  public class HiddenId
  {
    private char[] chars = new char[4];
    private Dictionary<ZeroWidthCharacter, object> value = new Dictionary<ZeroWidthCharacter, object>();

    /// <summary>
    /// 
    /// </summary>
    public HiddenId()
    {
      SetDefault();
    }

    /// <summary>
    /// 
    /// </summary>
    public Dictionary<ZeroWidthCharacter, object> Value
    {
      get { return value; }

      set
      {
        if (value.Count == 4)
        {
          this.value = value;
          ValueChanged();
        }
        else
          SetDefault();
      }
    }

    private void ValueChanged()
    {
      int index = 0;

      foreach (ZeroWidthCharacter c in (ZeroWidthCharacter[])Enum.GetValues(typeof(ZeroWidthCharacter)))
      {
        if (value.ContainsKey(c))
        {
          chars[index] = GetChar(c);
          index++;
        }

        if (index >= 4)
          break;
      }
    }

    private void SetDefault()
    {
      value.Clear();
      value.Add(ZeroWidthCharacter.NonJoiner, true);
      value.Add(ZeroWidthCharacter.Joiner, true);
      value.Add(ZeroWidthCharacter.PopDirectionalFormatting, true);
      value.Add(ZeroWidthCharacter.LeftToRightOverwrite, true);
      ValueChanged();
    }

    /// <summary>
    /// Encode an id.
    /// </summary>
    /// <param name="value">Id to be encoded.</param>
    /// <returns>Encoded id as a string.</returns>
    public string Encode(int value)
    {
      if (value == 0)
        return chars[0].ToString() + chars[0].ToString();

      var result = new StringBuilder();

      while (value > 0)
      {
        var digit = value%16;

        if (digit < 4)
        {
          result.Insert(0, chars[digit]);
          result.Insert(0, chars[0]);
        }
        else if (digit < 8)
        {
          result.Insert(0, chars[digit - 4]);
          result.Insert(0, chars[1]);
        }
        else if (digit < 12)
        {
          result.Insert(0, chars[digit - 8]);
          result.Insert(0, chars[2]);
        }
        else
        {
          result.Insert(0, chars[digit - 12]);
          result.Insert(0, chars[3]);
        }

        value = value / 16;
      }

      return result.ToString();
    }

    /// <summary>
    /// Decode an encoded id.
    /// </summary>
    /// <param name="value">Encoded id to be decoded.</param>
    /// <returns>Decoded id.</returns>
    public int Decode(string value)
    {
      if ((value == "") || (value.Length%2 != 0))
        return -1;

      var result = 0;

      while (value != "")
      {
        var digit = GetZeroWidthIndex(value[1]);

        if (value[0] == chars[1])
          digit += 4;
        else if (value[0] == chars[2])
          digit += 8;
        else if (value[0] == chars[3])
          digit += 12;

        result *= 16;
        result += digit;

        value = value.Substring(2);
      }

      return result;
    }

    /// <summary>
    /// Injects an id into a string.
    /// </summary>
    /// <param name="value">String where the id will be injected.</param>
    /// <param name="id">Id to be injected.</param>
    /// <returns>String with injected id.</returns>
    public string Inject(string value, int id)
    {
      return value + Encode(id);
    }

    /// <summary>
    /// Parse a string that may contain injected id(s).
    /// </summary>
    /// <param name="str">String that may contain injected id.</param>
    /// <returns>Array of decoded strings.</returns>
    public DecodedString[] Parse(string str)
    {
      var result = new List<DecodedString>();
      var value = new StringBuilder();
      var id = new StringBuilder();

      var i = 0;

      while (i < str.Length)
      {
        var c = str[i];

        if (IsZeroWidthChar(c))
          id.Append(c);
        else
        {
          if (id.Length > 0)
            Add(result, value, id);

          value.Append(c);
        }

        i++;
      }

      if (value.Length > 0)
        Add(result, value, id);

      return result.ToArray();
    }

    /// <summary>
    /// Parse a string that may contain injected id. If the string contains multiple strings return the first one.
    /// </summary>
    /// <param name="str">String that may contain injected id.</param>
    /// <returns>First decoded string.</returns>
    public DecodedString ParseFirst(string str)
    {
      var result = Parse(str);

      if (result.Length > 0)
        return result[0];
      else
        return null;
    }

    private static readonly char[] CHARS =
    {
      '\u200B',
      '\u200C',
      '\u200D',
      '\u200E',
      '\u202A',
      '\u202C',
      '\u202D',
      '\u2060',
      '\u2062',
      '\u2063',
      '\uFEFF'
    };

    /// <summary>
    /// 
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    public static char GetChar(ZeroWidthCharacter value)
    {
      return CHARS[(int)value];
    }


    private DecodedString Add(List<DecodedString> list, StringBuilder value, StringBuilder id)
    {
      var result = new DecodedString()
      {
        Value = value.ToString(),
        Id = Decode(id.ToString())
      };

      list.Add(result);

      value.Length = 0;
      id.Length = 0;

      return result;
    }

    private bool IsZeroWidthChar(char c)
    {
      foreach (var zeroWidthChar in chars)
      {
        if (c == zeroWidthChar)
          return true;
      }

      return false;
    }

    private int GetZeroWidthIndex(char c)
    {
      for (var i = 0; i < chars.Length; i++)
      {
        if (chars[i] == c)
          return i;
      }

      return 0;
    }
  }
}
