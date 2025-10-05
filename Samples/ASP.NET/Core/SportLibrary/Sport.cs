using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using Newtonsoft.Json;

// This model contain a sport. Because some properties of the sport need to be localized (e.g. name and origin)
// we split model into two entities: Sport and SpotLanguage. Sport contains language independet properties and 
// SportLanguage contains language specific properties. Sport has a list of SportLanguages, one for each language.
namespace Soluling.Sport
{
  // Enum that specifies Olympic Games status of the sport
  public enum Olympic
  {
    [Display(Name = "No")]
    No,

    [Display(Name = "Winter")]
    Winter,

    [Display(Name = "Summer")]
    Summer
  }

  // Enum that specifies if the sport is individual or team sport
  public enum SportKind
  {
    [Display(Name = "Individual")]
    Individual,

    [Display(Name = "Team")]
    Team
  }

  // Language indepented properties of Sport
  public class Sport
  {
    [Display(Name = "Id")]
    public int Id { get; set; }

    [JsonProperty(DefaultValueHandling = DefaultValueHandling.Include)]
    [Required]
    [Display(Name = "Olympic")]
    public Olympic Olympic { get; set; }

    // These are optional because individual sports do not use these properties
    [Display(Name = "Field players")]
    public int? FieldPlayers { get; set; }

    [Display(Name = "Goalie")]
    public bool? Goalie { get; set; }

    [NotMapped]
    [Display(Name = "Kind")]
    public SportKind Kind
    {
      get
      {
        if ((FieldPlayers == null) || (Goalie == null))
          return SportKind.Individual;
        else
          return SportKind.Team;
      }

      set
      {
        if (value == SportKind.Team)
        {
          FieldPlayers = 0;
          Goalie = true;
        }
        else
        {
          FieldPlayers = null;
          Goalie = null;
        }
      }
    }

    // List that contains the language specific parts
    public List<SportLanguage> Languages { get; set; } = new List<SportLanguage>();

    public Sport AddLanguage(SportLanguage language)
    {
      if (Languages == null)
        Languages = new List<SportLanguage>();

      language.SportId = Id;
      Languages.Add(language);

      return this;
    }

    // Moves the given language to the first item of in the language list
    public Sport MoveDefaultLanguateToTop(string value)
    {
      var index = Languages.FindIndex(s => s.Language == value);

      if (index == -1)
        index = Languages.FindIndex(s => s.Language == "en");

      if (index >= 0)
      {
        var item = Languages[index];
        Languages.RemoveAt(index);
        Languages.Insert(0, item);
      }

      return this;
    }
  }

  // Language specific properties of Sport
  public class SportLanguage
  {
    [Display(Name = "Id")]
    public int Id { get; set; }

    // IETF language tag: ll[l][-Ssss][-CC]
    [Required(ErrorMessage = "The Language field is required")]
    [Display(Name = "Language")]
    public string Language { get; set; }

    [Required]
    [Display(Name = "Name")]
    public string Name { get; set; }

    [Required]
    [Display(Name = "Origin")]
    public string Origin { get; set; }

    [Required]
    [Display(Name = "Description")]
    public string Description { get; set; }

    [Display(Name = "Machine translated")]
    public bool MachineTranslated { get; set; } = false;

    // We don't need foreign key property but having it turns on automatic cascade delete
    [JsonIgnore]
    public int SportId { get; set; }
  }
}