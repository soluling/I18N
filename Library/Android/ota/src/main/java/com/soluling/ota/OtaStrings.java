package com.soluling.ota;

import android.util.Xml;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The strings of downloaded Android resource files (values-fi/strings.xml): strings, plurals, and
 * string arrays by resource name. A string with markup (b, i, u, and other styling) is left out, so
 * the built-in one keeps its styling; xliff:g is kept as its text.
 */
final class OtaStrings
{
  final Map<String, String> strings = new HashMap<>();
  final Map<String, Map<String, String>> plurals = new HashMap<>();
  final Map<String, String[]> arrays = new HashMap<>();

  // Whether a downloaded string has the same placeholders as the built-in one, by name - checked
  // once, the first time the string is read
  final Map<String, Boolean> verified = new ConcurrentHashMap<>();

  /** Adds the resources of a strings file. Throws if the file is not valid XML. */
  void parse(InputStream stream) throws XmlPullParserException, IOException
  {
    XmlPullParser parser = Xml.newPullParser();
    parser.setInput(stream, null);

    int event = parser.getEventType();
    boolean complete = false;

    while (event != XmlPullParser.END_DOCUMENT)
    {
      // A file cut short (a broken download) ends before the end tag of its root
      if ((event == XmlPullParser.END_TAG) && (parser.getDepth() == 1))
        complete = true;

      if ((event == XmlPullParser.START_TAG) && (parser.getDepth() == 2))
      {
        String tag = parser.getName();
        String name = parser.getAttributeValue(null, "name");

        if ((name == null) || "false".equals(parser.getAttributeValue(null, "translatable")))
          skip(parser);
        else if ("string".equals(tag))
          putString(name, readValue(parser));
        else if ("plurals".equals(tag))
          readPlurals(parser, name);
        else if ("string-array".equals(tag))
          readArray(parser, name);
        else
          skip(parser);
      }

      event = parser.next();
    }

    if (!complete)
      throw new XmlPullParserException("The file is incomplete");
  }

  private void putString(String name, String value)
  {
    if (value != null)
      strings.put(name, value);
  }

  private void readPlurals(XmlPullParser parser, String name) throws XmlPullParserException, IOException
  {
    Map<String, String> items = new HashMap<>();
    int depth = parser.getDepth();

    while (next(parser, depth))
    {
      if ((parser.getEventType() == XmlPullParser.START_TAG) && "item".equals(parser.getName()))
      {
        String quantity = parser.getAttributeValue(null, "quantity");
        String value = readValue(parser);

        if ((quantity != null) && (value != null))
          items.put(quantity, value);
      }
      else if (parser.getEventType() == XmlPullParser.START_TAG)
        skip(parser);
    }

    // A plural without the other form can't be used for every count
    if (items.containsKey("other"))
      plurals.put(name, items);
  }

  private void readArray(XmlPullParser parser, String name) throws XmlPullParserException, IOException
  {
    List<String> items = new ArrayList<>();
    boolean valid = true;
    int depth = parser.getDepth();

    while (next(parser, depth))
    {
      if ((parser.getEventType() == XmlPullParser.START_TAG) && "item".equals(parser.getName()))
      {
        String value = readValue(parser);

        if (value == null)
          valid = false;
        else
          items.add(value);
      }
      else if (parser.getEventType() == XmlPullParser.START_TAG)
        skip(parser);
    }

    if (valid)
      arrays.put(name, items.toArray(new String[0]));
  }

  // Moves to the next event inside the element of the given depth; false at its end tag
  private static boolean next(XmlPullParser parser, int depth) throws XmlPullParserException, IOException
  {
    int event = parser.next();
    return (event != XmlPullParser.END_DOCUMENT) && !((event == XmlPullParser.END_TAG) && (parser.getDepth() == depth));
  }

  private static void skip(XmlPullParser parser) throws XmlPullParserException, IOException
  {
    int depth = parser.getDepth();

    while (next(parser, depth))
    {
      // Skip the content
    }
  }

  // Reads the value of the current element, which the parser is at the start tag of. Null if the
  // value has styling markup or refers to another resource.
  private static String readValue(XmlPullParser parser) throws XmlPullParserException, IOException
  {
    StringBuilder raw = new StringBuilder();
    boolean styled = false;
    int depth = parser.getDepth();

    while (next(parser, depth))
    {
      int event = parser.getEventType();

      if (event == XmlPullParser.TEXT)
        raw.append(parser.getText());
      else if (event == XmlPullParser.START_TAG)
      {
        // xliff:g marks text that is not translated; its content is part of the value
        String tag = parser.getName();

        if (!"g".equals(tag) && !tag.endsWith(":g"))
          styled = true;
      }
    }

    String text = raw.toString().trim();

    if (styled || text.startsWith("@") || text.startsWith("?"))
      return null;

    return unescape(raw.toString());
  }

  /**
   * Processes the value of a string resource as aapt does: the escapes \n, \t, \', \", \\, \@, \?,
   * and \\uXXXX, white space collapsed to one space and trimmed, except inside double quotes, which
   * are removed.
   */
  static String unescape(String raw)
  {
    StringBuilder result = new StringBuilder(raw.length());
    boolean quoted = false;
    boolean space = false;
    int length = raw.length();

    for (int i = 0; i < length; i++)
    {
      char c = raw.charAt(i);

      if ((c == '\\') && (i + 1 < length))
      {
        char escaped = raw.charAt(++i);
        String value;

        if (escaped == 'n')
          value = "\n";
        else if (escaped == 't')
          value = "\t";
        else if ((escaped == 'u') && (i + 4 < length))
        {
          try
          {
            value = String.valueOf((char) Integer.parseInt(raw.substring(i + 1, i + 5), 16));
            i += 4;
          }
          catch (NumberFormatException e)
          {
            value = "u";
          }
        }
        else
          value = String.valueOf(escaped);

        if (space)
          result.append(' ');

        space = false;
        result.append(value);
      }
      else if (c == '"')
      {
        if (space)
          result.append(' ');

        space = false;
        quoted = !quoted;
      }
      else if (!quoted && Character.isWhitespace(c))
        space = result.length() > 0;
      else
      {
        if (space)
          result.append(' ');

        space = false;
        result.append(c);
      }
    }

    return result.toString();
  }

  // %s, %d, %1$s, %.2f, %-10s, ... - Java's format specifiers. %% and %n aren't arguments.
  private static final Pattern PLACEHOLDER = Pattern.compile("%(?:(\\d+)\\$)?[-#+ 0,(<]*\\d*(?:\\.\\d+)?([a-zA-Z%])");

  /** The arguments of a format string as position:conversion, such as 1:s and 2:d */
  static Set<String> placeholders(String value)
  {
    Set<String> result = new TreeSet<>();
    Matcher matcher = PLACEHOLDER.matcher(value);
    int position = 0;

    while (matcher.find())
    {
      String conversion = matcher.group(2);

      if (conversion.equals("%") || conversion.equals("n"))
        continue;

      String index = matcher.group(1);
      int argument = (index != null) ? Integer.parseInt(index) : ++position;

      result.add(argument + ":" + conversion.toLowerCase());
    }

    return result;
  }
}
