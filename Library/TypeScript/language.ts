
/**
 * Enumeration that specifies the plural form. There are six different plural forms from singular to various plurals. 
 * How many forms a language uses depends on the language. Most languages only use singular and plural.
 */
export enum PluralForm 
{
  Zero,  // Nullar. Used when count is zero. Few languages support this but you can include a 0 pattern into your pattern string if you want to handle 0 in a different way.
  One,   // Singular. Used when count is one. Most languages support this.
  Two,   // Dual. Used when count is two. Few languages support this.
  Few,   // Trial, paucal, sexal, minority plural or plural-100. Used when count is few. Few languages support this. The range depends on the language. Most often this is something between 2 and 4.
  Many,  // Greater paucal. Used when count is many. Few languages support this. The range depends on the language. Most often this is more than 4.
  Other  // Plural. Used when count does not belong to any other group. All languages support this. Most often this is the plural form.
};

/**
 * Enumeration that specifies the gender form. There are three different gender forms. 
 * How many forms a language uses depends on the language. Many languages do not use gender so they only use other.
 */
export enum Gender
{
  Male,    // Used with male words.
  Female,  // Used with femlate words.
  Neutral  // Other or neutral. Used when language does not use gender or gender is neutral. Most languages support this.</summary>
};

export class Language
{
  protected static locale: string = "en";

  /**
   * Set the active language.
   * @param {string} value The language or locale id.
   */
  public static SetLanguage(value: string): void
  {
    this.locale = value;
  }
  
  /**
   * Get the decimal separator.
   */
  public static GetDecimalSeparator(): string
  {
    // 1.1 -> "1.1" -> ".1" -> "."
    // 1.1 -> "1,1" -> ",1" -> ","
    var str = new Intl.NumberFormat(this.locale).format(1.1);
    var result = str.replace(/[\d-]/, "");
    return result.substring(0, 1);
  }
}