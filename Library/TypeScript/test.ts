import { Gender } from "./language";
import { Ordinal } from "./ordinal";
import { Plural } from "./plural";
import { PluralSprintf } from "./pluralsprintf";
import { Plurali18next } from "./plurali18next";
import * as i18next from "i18next";
//import * as i18nextXhrBackend from "i18next-xhr-backend";

const LANGUAGE = "en";

i18next
//.use(i18nextXhrBackend)
.init({
  "debug": true,
  "lng": LANGUAGE,
  resources: 
  {
    en: 
    {
      translation: 
      {
        "key": "Sample",
        "somebody": "Somebody",
        "male": "John",
        "female": "Jill",
        "pluralInterpolation": "one;I have one ski;other;I have {{ski}} skis",
        "pluralSprintf": "one;I have one ski;other;I have %d skis",
        "zeroInterpolation": "zero;I have no skis;one;I have one ski;other;I have {{ski}} skis",
        "zeroSprintf": "zero;I have no skis;one;I have one ski;other;I have %d skis",
        "genderInterpolation": "male;{{name}} will bring his skis;female;{{name}} will bring her skis",
        "genderSprintf": "male;%s will bring his skis;female;%s will bring her skis",
        "multiInterpolation": "male;{{name}} will bring his {{next}};female;{{name}} will bring her {{next}};next;one;car.;other;{{car}} cars.",
        "multiSprintf": "male;%s will bring his %s;female;%s will bring her %s;next;one;car.;other;%d cars."
      }
    },
    fi: 
    {
      translation: 
      {
        "key": "Esimerkki",
        "somebody": "Joku",
        "male": "Jouni",
        "female": "Jaana",
        "pluralInterpolation": "one;Minulla on yksi suksi;other;Minulla on {{ski}} suksea",
        "pluralSprintf": "one;Minulla on yksi suksi;other;Minulla on %d suksea",
        "zeroInterpolation": "zero;Minulla ei ole suksia;one;Minulla on yksi suksi;other;Minulla on {{ski}} suksea",
        "zeroSprintf": "zero;Minulla ei ole suksia;one;Minulla on yksi suksi;other;Minulla on %d suksea",
        "genderInterpolation": "neutral;{{name}} tuo sukset",
        "genderSprintf": "neutral;%s tuo sukset",
        "multiInterpolation": "neutral;{{name}} tuo {{next}};next;one;autonsa.;other;{{car}} autoaan.",
        "multiSprintf": "neutral;%s tuo %s;next;one;autonsa.;other;%d autoaan."
      }
    }
  }
});

var pluralInterpolation = Plurali18next.t("pluralInterpolation", "one;I have one ski;other;I have {{ski}} skis");
var pluralSprintf = i18next.t("pluralSprintf", "one;I have one ski;other;I have %d skis");
var genderInterpolation = Plurali18next.t("genderInterpolation", "male;{{name}} will bring his skis;female;{{name}} will bring her skis");
var genderSprintf = i18next.t("genderSprintf", "male;%s will bring his skis;female;%s will bring her skis");
var multiInterpolation  = Plurali18next.t("multiInterpolation", "male;{{name}} will bring his {{next}};female;{{name}} will bring her {{next}};next;one;car.;other;{{car}} cars.");
var multiSprintf = i18next.t("multiSprintf", "male;%s will bring his %s;female;%s will bring her %s;next;one;car.;other;%d cars.");
var somebody = i18next.t("somebody", "Somebody");
var male = i18next.t("male", "John");
var female = i18next.t("female", "Jill");

function Log(str: string)
{
  console.log(str);
}

function Header(str: String)
{
  Log("");
  Log(str + ":");
}

Plural.SetLanguage(LANGUAGE);

Header("Plain interpolation")
Log(Plural.Interpolate("I have {{ski}} skis", { ski: 0 }));
Log(Plural.Interpolate("I have {{ ski }} ski", { ski: 1 }));
Log(Plural.Interpolate("I have {{ski }} skis", { ski: 2 }));
Log(Plural.Interpolate("I have {{ ski}} skis", { ski: 3 }));

Log(Plural.Interpolate("I have {{ski}} skis and {{car}} cars", { ski: 3, car: 2 }));

Header("Interpolation format");
Log(Plural.FormatPlural(pluralInterpolation, { ski: 0 }));
Log(Plural.FormatPlural(pluralInterpolation, { ski: 1 }));
Log(Plural.FormatPlural(pluralInterpolation, { ski: 2 }));

Log(Plural.FormatGender(genderInterpolation, Gender.Male, { name: male }));
Log(Plural.FormatGender(genderInterpolation, Gender.Female, { name: female }));
Log(Plural.FormatGender(genderInterpolation, Gender.Neutral, { name: somebody }));

Log(Plural.Format(multiInterpolation, { name: male, gender: Gender.Male }, { car: 0 }));
Log(Plural.Format(multiInterpolation, { name: male, gender: Gender.Male }, { car: 1 }));
Log(Plural.Format(multiInterpolation, { name: male, gender: Gender.Male }, { car: 2 }));
Log(Plural.Format(multiInterpolation, { name: female, gender: Gender.Female }, { car: 0 }));
Log(Plural.Format(multiInterpolation, { name: female, gender: Gender.Female }, { car: 1 }));
Log(Plural.Format(multiInterpolation, { name: female, gender: Gender.Female }, { car: 2 }));
Log(Plural.Format(multiInterpolation, { name: somebody, gender: Gender.Neutral }, { car: 1 }));

Header("sprintf format");
Log(PluralSprintf.FormatPlural(pluralSprintf, 0));
Log(PluralSprintf.FormatPlural(pluralSprintf, 1));
Log(PluralSprintf.FormatPlural(pluralSprintf, 2));

Log(PluralSprintf.FormatGender(genderSprintf, Gender.Male, male));
Log(PluralSprintf.FormatGender(genderSprintf, Gender.Female, female));
Log(PluralSprintf.FormatGender(genderSprintf, Gender.Neutral, somebody));

Log(PluralSprintf.Format(multiSprintf, { value: male, gender: Gender.Male }, 0));
Log(PluralSprintf.Format(multiSprintf, { value: male, gender: Gender.Male }, 1));
Log(PluralSprintf.Format(multiSprintf, { value: male, gender: Gender.Male }, 2));
Log(PluralSprintf.Format(multiSprintf, { value: female, gender: Gender.Female }, 0));
Log(PluralSprintf.Format(multiSprintf, { value: female, gender: Gender.Female }, 1));
Log(PluralSprintf.Format(multiSprintf, { value: female, gender: Gender.Female }, 2));
Log(PluralSprintf.Format(multiSprintf, { value: somebody, gender: Gender.Neutral }, 1));