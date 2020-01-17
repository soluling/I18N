using System.Linq;
using Soluling.Sport;

namespace SportAPI.Models
{
  public static class SeedData
  {
    // Populates the databases to contain initial sports in two languages: English and Finnish
    public static void Initialize(SportContext context)
    {
      if (context.Sport.Any())
        return;

      context.Sport.AddRange
      (
        // Soccer in English and Finnish
        new Sport
        {
          Olympic = Olympic.Summer,
          FieldPlayers = 10,
          Goalie = true
        }
        .AddLanguage(new SportLanguage
        {
          Language = "en",
          Name = "Soccer",
          Origin = "England",
          Description = "Soccer is a sport played between two teams of eleven players with a spherical ball"
        })
        .AddLanguage(new SportLanguage
        {
          Language = "fi",
          Name = "Jalkapallo",
          Origin = "Englanti",
          Description = "Jalkapallo on urheilulaji, jossa kaksi yksitoistahenkistä joukkuetta pelaavat pallolla."
        }),

        // Hockey in English and Finnish
        new Sport
        {
          Olympic = Olympic.Winter,
          FieldPlayers = 5,
          Goalie = true
        }
        .AddLanguage(new SportLanguage
        {
          Language = "en",
          Name = "Ice hockey",
          Origin = "Canada",
          Description = "Ice hockey is a team sport played on ice, in which skaters use sticks to direct a puck into the opposing team's goal"
        })
        .AddLanguage(new SportLanguage
        {
          Language = "fi",
          Name = "Jääkiekko",
          Origin = "Kanada",
          Description = "Jääkiekko on jäällä pelattava joukkuelaji, jossa luistelijat käyttävät mailaa saadakseen kiekon vastajoukkueen maaliin"
        }),

        // Basketball in English and Finnish
        new Sport
        {
          Olympic = Olympic.Summer,
          FieldPlayers = 5,
          Goalie = false
        }
        .AddLanguage(new SportLanguage
        {
          Language = "en",
          Name = "Basketball",
          Origin = "United States",
          Description = "Basketball is a team sport in which two teams of five players try to score points by throwing a ball through the top of a basketball hoop while following a set of rules"
        })
        .AddLanguage(new SportLanguage
        {
          Language = "fi",
          Name = "Koripallo",
          Origin = "Yhdysvallat",
          Description = "Koripallo on joukkuelaji, jossa kaksi viisihenkistä joukkuetta yrittää tehdä pisteitä heittämällä pallon korin läpi samalla noudattaen sääntöjä"
        }),

        // Skiing in English and Finnish
        new Sport
        {
          Olympic = Olympic.Winter
        }
        .AddLanguage(new SportLanguage
        {
          Language = "en",
          Name = "Alpine skiing",
          Origin = "Norway",
          Description = "Alpine skiing is the sport or recreation of sliding down snow-covered hills on skis with fixed-heel bindings"
        })
        .AddLanguage(new SportLanguage
        {
          Language = "fi",
          Name = "Alppihiihto",
          Origin = "Norja",
          Description = "Alppihiihto on laskettelulaji ja talviurheilumuoto, jossa lasketaan lumipeitteisiä rinteitä alas käyttäen kahta suksea, jäykkää monoa ja sidettä, jossa koko jalkapöytä lukittuu sukseen kiinni"
        })
      );

      context.SaveChanges();
    }
  }
}