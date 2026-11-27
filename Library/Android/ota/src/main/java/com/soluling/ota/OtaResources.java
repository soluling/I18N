package com.soluling.ota;

import android.content.res.Resources;
import android.icu.text.PluralRules;

import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Resources that return the downloaded strings of the configuration's language, and the built-in
 * ones for everything else. A downloaded string is used only if its placeholders are the built-in
 * string's, so an older application never gets a translation that its code would format wrongly.
 * Only the application's own resources are replaced, not Android's (android.R).
 */
@SuppressWarnings("deprecation")
final class OtaResources extends Resources
{
  // The package id of the application's resources, including those of its libraries
  private static final int APPLICATION_PACKAGE = 0x7f;

  // Counts that give every plural form of every language, for the placeholders of a built-in plural
  private static final int[] PLURAL_SAMPLES = { 0, 1, 2, 3, 4, 5, 6, 7, 11, 12, 21, 22, 100, 101, 102, 1000000 };

  private final Map<Integer, String> names = new ConcurrentHashMap<>();

  OtaResources(Resources base)
  {
    super(base.getAssets(), base.getDisplayMetrics(), base.getConfiguration());
  }

  private Locale locale()
  {
    return getConfiguration().getLocales().get(0);
  }

  private OtaStrings strings(int id)
  {
    if ((id >>> 24) != APPLICATION_PACKAGE)
      return null;

    OtaStore store = SolulingOta.store();
    return (store != null) ? store.getStrings(locale()) : null;
  }

  private String name(int id)
  {
    String result = names.get(id);

    if (result == null)
    {
      try
      {
        result = getResourceEntryName(id);
        names.put(id, result);
      }
      catch (NotFoundException e)
      {
        return null;
      }
    }

    return result;
  }

  /** The downloaded string of a string resource, or null if there is none or it can't be used */
  String findString(int id)
  {
    OtaStrings strings = strings(id);
    String name = (strings != null) ? name(id) : null;
    String value = (name != null) ? strings.strings.get(name) : null;

    if (value == null)
      return null;

    Boolean valid = strings.verified.get(name);

    if (valid == null)
    {
      try
      {
        valid = OtaStrings.placeholders(value).equals(OtaStrings.placeholders(super.getText(id).toString()));
      }
      catch (NotFoundException e)
      {
        valid = false;
      }

      strings.verified.put(name, valid);
    }

    return valid ? value : null;
  }

  // The downloaded form of a plural resource for the count, or null
  private String findQuantityString(int id, int quantity)
  {
    OtaStrings strings = strings(id);
    String name = (strings != null) ? name(id) : null;
    Map<String, String> items = (name != null) ? strings.plurals.get(name) : null;

    if (items == null)
      return null;

    String value = items.get(PluralRules.forLocale(locale()).select(quantity));

    if (value == null)
      value = items.get("other");

    if (value == null)
      return null;

    // Valid if every form has only placeholders that the built-in plural has in some form
    String key = "plurals/" + name;
    Boolean valid = strings.verified.get(key);

    if (valid == null)
    {
      try
      {
        Set<String> builtIn = new HashSet<>();

        for (int sample : PLURAL_SAMPLES)
          builtIn.addAll(OtaStrings.placeholders(super.getQuantityText(id, sample).toString()));

        valid = true;

        for (String item : items.values())
          valid = valid && builtIn.containsAll(OtaStrings.placeholders(item));
      }
      catch (NotFoundException e)
      {
        valid = false;
      }

      strings.verified.put(key, valid);
    }

    return valid ? value : null;
  }

  // The downloaded string array, or null if there is none or it has another number of items
  private String[] findArray(int id)
  {
    OtaStrings strings = strings(id);
    String name = (strings != null) ? name(id) : null;
    String[] value = (name != null) ? strings.arrays.get(name) : null;

    if (value == null)
      return null;

    try
    {
      return (value.length == super.getTextArray(id).length) ? value.clone() : null;
    }
    catch (NotFoundException e)
    {
      return null;
    }
  }

  @Override
  public CharSequence getText(int id) throws NotFoundException
  {
    String value = findString(id);
    return (value != null) ? value : super.getText(id);
  }

  @Override
  public CharSequence getText(int id, CharSequence def)
  {
    String value = findString(id);
    return (value != null) ? value : super.getText(id, def);
  }

  @Override
  public String getString(int id) throws NotFoundException
  {
    String value = findString(id);
    return (value != null) ? value : super.getString(id);
  }

  @Override
  public String getString(int id, Object... formatArgs) throws NotFoundException
  {
    String value = findString(id);
    return (value != null) ? String.format(locale(), value, formatArgs) : super.getString(id, formatArgs);
  }

  @Override
  public CharSequence getQuantityText(int id, int quantity) throws NotFoundException
  {
    String value = findQuantityString(id, quantity);
    return (value != null) ? value : super.getQuantityText(id, quantity);
  }

  @Override
  public String getQuantityString(int id, int quantity) throws NotFoundException
  {
    String value = findQuantityString(id, quantity);
    return (value != null) ? value : super.getQuantityString(id, quantity);
  }

  @Override
  public String getQuantityString(int id, int quantity, Object... formatArgs) throws NotFoundException
  {
    String value = findQuantityString(id, quantity);
    return (value != null) ? String.format(locale(), value, formatArgs) : super.getQuantityString(id, quantity, formatArgs);
  }

  @Override
  public String[] getStringArray(int id) throws NotFoundException
  {
    String[] value = findArray(id);
    return (value != null) ? value : super.getStringArray(id);
  }

  @Override
  public CharSequence[] getTextArray(int id) throws NotFoundException
  {
    String[] value = findArray(id);
    return (value != null) ? value : super.getTextArray(id);
  }
}
