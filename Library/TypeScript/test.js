"use strict";
var language_1 = require("./language");
var plural_1 = require("./plural");
var pluralsprintf_1 = require("./pluralsprintf");
var plurali18next_1 = require("./plurali18next");
var i18next = require("i18next");
//import * as i18nextXhrBackend from "i18next-xhr-backend";
var LANGUAGE = "en";
i18next
    .init({
    "debug": true,
    "lng": LANGUAGE,
    resources: {
        en: {
            translation: {
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
        fi: {
            translation: {
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
var pluralInterpolation = plurali18next_1.Plurali18next.t("pluralInterpolation", "one;I have one ski;other;I have {{ski}} skis");
var pluralSprintf = i18next.t("pluralSprintf", "one;I have one ski;other;I have %d skis");
var genderInterpolation = plurali18next_1.Plurali18next.t("genderInterpolation", "male;{{name}} will bring his skis;female;{{name}} will bring her skis");
var genderSprintf = i18next.t("genderSprintf", "male;%s will bring his skis;female;%s will bring her skis");
var multiInterpolation = plurali18next_1.Plurali18next.t("multiInterpolation", "male;{{name}} will bring his {{next}};female;{{name}} will bring her {{next}};next;one;car.;other;{{car}} cars.");
var multiSprintf = i18next.t("multiSprintf", "male;%s will bring his %s;female;%s will bring her %s;next;one;car.;other;%d cars.");
var somebody = i18next.t("somebody", "Somebody");
var male = i18next.t("male", "John");
var female = i18next.t("female", "Jill");
function Log(str) {
    console.log(str);
}
function Header(str) {
    Log("");
    Log(str + ":");
}
plural_1.Plural.SetLanguage(LANGUAGE);
Header("Plain interpolation");
Log(plural_1.Plural.Interpolate("I have {{ski}} skis", { ski: 0 }));
Log(plural_1.Plural.Interpolate("I have {{ ski }} ski", { ski: 1 }));
Log(plural_1.Plural.Interpolate("I have {{ski }} skis", { ski: 2 }));
Log(plural_1.Plural.Interpolate("I have {{ ski}} skis", { ski: 3 }));
Log(plural_1.Plural.Interpolate("I have {{ski}} skis and {{car}} cars", { ski: 3, car: 2 }));
Header("Interpolation format");
Log(plural_1.Plural.FormatPlural(pluralInterpolation, { ski: 0 }));
Log(plural_1.Plural.FormatPlural(pluralInterpolation, { ski: 1 }));
Log(plural_1.Plural.FormatPlural(pluralInterpolation, { ski: 2 }));
Log(plural_1.Plural.FormatGender(genderInterpolation, language_1.Gender.Male, { name: male }));
Log(plural_1.Plural.FormatGender(genderInterpolation, language_1.Gender.Female, { name: female }));
Log(plural_1.Plural.FormatGender(genderInterpolation, language_1.Gender.Neutral, { name: somebody }));
Log(plural_1.Plural.Format(multiInterpolation, { name: male, gender: language_1.Gender.Male }, { car: 0 }));
Log(plural_1.Plural.Format(multiInterpolation, { name: male, gender: language_1.Gender.Male }, { car: 1 }));
Log(plural_1.Plural.Format(multiInterpolation, { name: male, gender: language_1.Gender.Male }, { car: 2 }));
Log(plural_1.Plural.Format(multiInterpolation, { name: female, gender: language_1.Gender.Female }, { car: 0 }));
Log(plural_1.Plural.Format(multiInterpolation, { name: female, gender: language_1.Gender.Female }, { car: 1 }));
Log(plural_1.Plural.Format(multiInterpolation, { name: female, gender: language_1.Gender.Female }, { car: 2 }));
Log(plural_1.Plural.Format(multiInterpolation, { name: somebody, gender: language_1.Gender.Neutral }, { car: 1 }));
Header("sprintf format");
Log(pluralsprintf_1.PluralSprintf.FormatPlural(pluralSprintf, 0));
Log(pluralsprintf_1.PluralSprintf.FormatPlural(pluralSprintf, 1));
Log(pluralsprintf_1.PluralSprintf.FormatPlural(pluralSprintf, 2));
Log(pluralsprintf_1.PluralSprintf.FormatGender(genderSprintf, language_1.Gender.Male, male));
Log(pluralsprintf_1.PluralSprintf.FormatGender(genderSprintf, language_1.Gender.Female, female));
Log(pluralsprintf_1.PluralSprintf.FormatGender(genderSprintf, language_1.Gender.Neutral, somebody));
Log(pluralsprintf_1.PluralSprintf.Format(multiSprintf, { value: male, gender: language_1.Gender.Male }, 0));
Log(pluralsprintf_1.PluralSprintf.Format(multiSprintf, { value: male, gender: language_1.Gender.Male }, 1));
Log(pluralsprintf_1.PluralSprintf.Format(multiSprintf, { value: male, gender: language_1.Gender.Male }, 2));
Log(pluralsprintf_1.PluralSprintf.Format(multiSprintf, { value: female, gender: language_1.Gender.Female }, 0));
Log(pluralsprintf_1.PluralSprintf.Format(multiSprintf, { value: female, gender: language_1.Gender.Female }, 1));
Log(pluralsprintf_1.PluralSprintf.Format(multiSprintf, { value: female, gender: language_1.Gender.Female }, 2));
Log(pluralsprintf_1.PluralSprintf.Format(multiSprintf, { value: somebody, gender: language_1.Gender.Neutral }, 1));
//# sourceMappingURL=test.js.map