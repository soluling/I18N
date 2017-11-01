import { Plural } from "./plural";
import * as i18next from "i18next";

/**
 * Implements function for i18next.
 */
export class Plurali18next extends Plural
{
  /**
   * Gets an i18next resource without performing interpolation.
   * @param {string} key - The i18next key of a multi-pattern strings resource.
   * @param {string} value - The original value.
   * @return {string} The plain result string.  
   */
  public static t(key: string, value: string): string
  {
    var options = 
    {
      defaultValue: value, 
      interpolation: 
      { 
        prefix: "{{{{", 
        suffix: "}}}}" 
      }
    };

    return i18next.t(key, options);
  }
}