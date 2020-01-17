namespace RazorSport.Data
{
  public class IndividualButtonPartial
  {
    public int? Id { get; set; }
    public string Page { get; set; }
    public string Glyph { get; set; }
    public string ButtonType { get; set; }
    public bool CanBeDeleted { get; set; } = true;

    public string ButtonStyle
    {
      get
      {
        var result = ButtonType;

        if (!CanBeDeleted)
          result += " disabled";

        return result;
      }
    }

    public string ActionParameters
    {
      get
      {
        if ((Id >= 0) && (Id != null))
          return Id.ToString();
        else
          return null;
      }
    }
  }
}
