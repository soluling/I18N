namespace Soluling
{
  /// <summary>
  /// 
  /// </summary>
  public static class PatternNames
  {
    /// <summary>
    /// Get the name of the plural form.
    /// </summary>
    /// <param name="plural">Plural form</param>
    /// <returns>Name of the plural form</returns>
    public static string GetPluralName(Plural plural)
    {
      switch (plural)
      {
        case Plural.Zero:
          return Resources.Zero;

        case Plural.One:
          return Resources.One;

        case Plural.Two:
          return Resources.Two;

        case Plural.Few:
          return Resources.Few;

        case Plural.Many:
          return Resources.Many;

        default:
          return Resources.Plural;
      }
    }

    /// <summary>
    /// Get the name of the gender form.
    /// </summary>
    /// <param name="gender">Gender form</param>
    /// <returns>Name of the gender form</returns>
    public static string GetGenderName(Gender gender)
    {
      switch (gender)
      {
        case Gender.Male:
          return Resources.Male;

        case Gender.Female:
          return Resources.Female;

        default:
          return Resources.Neutral;
      }
    }
  }
}
