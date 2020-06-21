import { PluralForm, Gender } from "./language";
import { Plural, FormatString } from "./plural";
import { sprintf } from "sprintf-js";

/**
 * Implements multi pattern format using sprintf-js.
 * This class uses the same format as printf of C language. If you are more familiar to that format than the modern interpolation use this class.
 */
export class PluralSprintf extends Plural
{
  /**
   * Selects the right plural pattern from the passed multi pattern string and applies that for sprintf function.
   * @param {string} patterns - A multi pattern string.
   * @param {number} count - Count value used to detemine the plural form.
   * @return {string} The result string where parameter(s) were injected to the placeholder(s).  
   */
  public static FormatPlural(patterns: string, count: number): string
  {
    return this.FormatPluralEx(patterns, count, count);
  }

  /**
   * Selects the right plural pattern from the passed multi pattern string and applies that for sprintf function.
   * @param {string} patterns - A multi pattern string.
   * @param {number} count - Count value used to detemine the plural form. count parameter is not automatically passed to sprintf but you have to include it to args.
   * @param {any} args - Optional additional arguments for sprtinf.
   * @return {string} The result string where parameter(s) were injected to the placeholder(s).  
   */
  public static FormatPluralEx(patterns: string, count: number, ...args: any[]): string
  {
    var pattern = this.GetPluralPattern(patterns, count);
    return sprintf(pattern, args);
  }

  /**
   * Selects the right gender pattern from the passed multi pattern string and applies that for sprintf function.
   * @param {string} patterns - A multi pattern string.
   * @param {Gender} gender - Gender value.
   * @param {any} args - Optional arguments for sprtinf.
   * @return {string} The result string where parameter(s) were injected to the placeholder(s).  
   */
  public static FormatGender(patterns: string, gender: Gender, ...args: any[]): string
  {
    var pattern = this.GetGenderPattern(patterns, gender);
    return sprintf(pattern, args);
  }

  /**
   * Selects the right gender pattern from the passed multi pattern string and applies that for sprintf function.
   * @param {string} patterns - A multi pattern string.
   * @param {any} args - Arguments for sprtinf. For plural parameter pass the number. For gender parameter pass an object containing gender and value properties.
   * @return {string} The result string where parameter(s) were injected to the placeholder(s).  
   */
  public static Format(patterns: string, ...args: any[]): string
  {
    this.InitializeIndexProc();

    var result: string = "";
    var str: FormatString = new FormatString(patterns);
    var count: number = args.length;

    if (count != str.Count)
      throw new Error(sprintf("Pattern \"%s\" does not have enough patterns for %d parameter", patterns, count));

    // Process parameter from last to first and build the result string part by part
    for (var i = str.Count - 1; i >= 0; i--)
    {
      var parameter = str.GetItem(i);
      var arg = args[i];

      if (typeof arg == "number")
      {
        // Plural
        var count: number = arg;
        var form: PluralForm = Plural.indexProc(count);
        var pattern: string = parameter.GetCheckedPluralString(form, count);

        if (i == str.Count - 1)
          result = sprintf(pattern, count);
        else
          result = sprintf(pattern, count, result);
      } 
      else
      {
        // Gender
        var keys = Object.keys(arg);
        var value = undefined;
        var gender: Gender = arg["gender"];

        if (gender == undefined)
          throw new Error("No gender parameter passed for: " + patterns);
        
        for (var key of keys)
        {
          if (key != "gender")
          {
            value = arg[key];
            break;
          }
        }

        if (value == undefined)
          throw new Error("Gender parameter does not have value parameter");

        var pattern = parameter.GetGenderString(gender);

        if (pattern == "")
          pattern = parameter.DefaultValue;

        if (i == str.Count - 1)
          result = sprintf(pattern, value);
        else
          result = sprintf(pattern, value, result);
      }
    }
    
    if (str.StartPattern != "")
      result = sprintf(str.StartPattern, result);

    return result;
  }
}