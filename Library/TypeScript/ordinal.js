"use strict";
var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var language_1 = require("./language");
/**
 * Enumeration that specifies the string form of an ordinal.
 */
(function (OrdinalStringForm) {
    OrdinalStringForm[OrdinalStringForm["Long"] = 0] = "Long";
    OrdinalStringForm[OrdinalStringForm["Short"] = 1] = "Short"; // Short string form such as "1st" and "4th"
})(exports.OrdinalStringForm || (exports.OrdinalStringForm = {}));
var OrdinalStringForm = exports.OrdinalStringForm;
/**
 * Enumeration that specifies what plural group a language belongs to.
 * The algorithm is named based on the main language of the group.
 * For example PluralGroup.Japanese is not only applied to Japanese but all the other languages where plural rules are the same as in Japanese (expecially Asian ones).
 */
(function (SmallOrdinal) {
    SmallOrdinal[SmallOrdinal["First"] = 1] = "First";
    SmallOrdinal[SmallOrdinal["Second"] = 2] = "Second";
    SmallOrdinal[SmallOrdinal["Third"] = 3] = "Third";
    SmallOrdinal[SmallOrdinal["Fourth"] = 4] = "Fourth";
    SmallOrdinal[SmallOrdinal["Fifth"] = 5] = "Fifth";
    SmallOrdinal[SmallOrdinal["Sixth"] = 6] = "Sixth";
    SmallOrdinal[SmallOrdinal["Seventh"] = 7] = "Seventh";
    SmallOrdinal[SmallOrdinal["Eighth"] = 8] = "Eighth";
    SmallOrdinal[SmallOrdinal["Ninth"] = 9] = "Ninth";
    SmallOrdinal[SmallOrdinal["Tenth"] = 10] = "Tenth"; // tenth or 10th
})(exports.SmallOrdinal || (exports.SmallOrdinal = {}));
var SmallOrdinal = exports.SmallOrdinal;
;
var OrdinalData = (function () {
    function OrdinalData() {
    }
    return OrdinalData;
}());
function initialize(target) {
    target.initialize();
}
/**
 * Implements a formatting a number into its ordinal form.
 *
 * For example in English "This is my 1st car" and "This is my 2nd card". Note the ending after the number.
 * It is either st, nd, rd, or th depending on the number.
 * Some other countries use different endings and the rules vary. Some countries just add pediod in the end
 * like in Finnish "Tämä on minun 1. autoni" or "Tämä on minun 2. autoni".
 */
var Ordinal = (function (_super) {
    __extends(Ordinal, _super);
    function Ordinal() {
        _super.apply(this, arguments);
    }
    /**
     * Converts an ordinal number to a string such as "1st" or "first".
     * @param form String form to be used.
     * @param ordinal Ordinal number.
     * @param plural Specifies the plural form.
     * @param gender Specifies the gender.
     * @return {string} The formatted string.
     */
    Ordinal.Format = function (form, ordinal, plural, gender) {
        if (plural === void 0) { plural = language_1.PluralForm.One; }
        if (gender === void 0) { gender = language_1.Gender.Neutral; }
        if (form == OrdinalStringForm.Short)
            return Ordinal.FormatShort(ordinal, plural, gender);
        else
            return Ordinal.FormatLong(ordinal, plural, gender);
    };
    /**
     * Converts an ordinal number to a string using the short format such as "1st" and "4th".
     * @param ordinal Ordinal number.
     * @param plural Specifies the plural form.
     * @param gender Specifies the gender.
     * @return {string} The formatted string.
     */
    Ordinal.FormatShort = function (ordinal, plural, gender) {
        if (plural === void 0) { plural = language_1.PluralForm.One; }
        if (gender === void 0) { gender = language_1.Gender.Neutral; }
        return this.GetData().ShortProc(ordinal, plural, gender);
    };
    /**
     * Converts an ordinal number to a string using the long format such as "first" and "fourth".
     * Only the first 10 ordinals can be formatted to the long format. The rest of the ordinals will always
     * be formatted using the short format such as "21st" and "24th".
     * @param ordinal Ordinal number.
     * @param plural Specifies the plural form.
     * @param gender Specifies the gender.
     * @return {string} The formatted string.
     */
    Ordinal.FormatLong = function (ordinal, plural, gender) {
        if (plural === void 0) { plural = language_1.PluralForm.One; }
        if (gender === void 0) { gender = language_1.Gender.Neutral; }
        if (this.IsShortOrdinal(ordinal))
            return this.GetData().LongProc(ordinal, plural, gender);
        else
            return this.FormatShort(ordinal, plural, gender);
    };
    /**
     * Register formatting functions for a language.
     * @param id ISO 639-1 language code of the language.
     * @param longProc Function that converts a small ordianal to a long string.
     * @param shortProc Function that converts an ordianal to a short string.
     */
    Ordinal.Register = function (id, longProc, shortProc) {
        var data = this.datas[id];
        if (data == null) {
            data = new OrdinalData();
            this.datas[id] = data;
        }
        data.LongProc = longProc;
        data.ShortProc = shortProc;
    };
    Ordinal.GetData = function () {
        var id = this.locale;
        if (this.datas[id] == null)
            id = "";
        return this.datas[id];
    };
    Ordinal.IsShortOrdinal = function (value) {
        return (value >= SmallOrdinal.First) && (value <= SmallOrdinal.Tenth);
    };
    Ordinal.GetDefaultShort = function (ordinal, plural, gender) {
        return ordinal.toString();
    };
    Ordinal.GetDefaultLong = function (ordinal, plural, gender) {
        return ordinal.toString();
    };
    Ordinal.GetPeriodShort = function (ordinal, plural, gender) {
        return ordinal.toString() + ".";
    };
    // English
    Ordinal.GetEnglishShort = function (ordinal, plural, gender) {
        var str;
        if ((ordinal % 10 == 1) && (ordinal % 100 != 11))
            str = "st";
        else if ((ordinal % 10 == 2) && (ordinal % 100 != 12))
            str = "nd";
        else if ((ordinal % 10 == 3) && (ordinal % 100 != 13))
            str = "rd";
        else
            str = "th";
        return ordinal.toString() + str;
    };
    Ordinal.GetEnglishLong = function (ordinal, plural, gender) {
        var ENGLISH = [
            "",
            "first",
            "second",
            "third",
            "fourth",
            "fifth",
            "sixth",
            "seventh",
            "eighth",
            "ninth",
            "tenth"
        ];
        return ENGLISH[ordinal];
    };
    // German
    Ordinal.GetGermanLong = function (ordinal, plural, gender) {
        var GERMAN = [
            "",
            "erstens",
            "zweitens",
            "drittens",
            "viertens",
            "fünftens",
            "sechstens",
            "siebtens",
            "achtens",
            "neuntens",
            "zehntens"
        ];
        if (ordinal == SmallOrdinal.First) {
            if (plural == language_1.PluralForm.Other)
                return "ersten";
            else if (gender == language_1.Gender.Neutral)
                return "erstes";
            else if (gender == language_1.Gender.Female)
                return "erste";
            else if (gender == language_1.Gender.Male)
                return "erster";
            else
                return GERMAN[ordinal];
        }
        else if (ordinal == SmallOrdinal.Second) {
            if (plural == language_1.PluralForm.Other)
                return "zweiten";
            else if (gender == language_1.Gender.Neutral)
                return "zweites";
            else if (gender == language_1.Gender.Female)
                return "zweite";
            else if (gender == language_1.Gender.Male)
                return "zweiter";
            else
                return GERMAN[ordinal];
        }
        else if (ordinal == SmallOrdinal.Third) {
            if (plural == language_1.PluralForm.Other)
                return "dritten";
            else if (gender == language_1.Gender.Neutral)
                return "drittes";
            else if (gender == language_1.Gender.Female)
                return "dritte";
            else if (gender == language_1.Gender.Male)
                return "dritter";
            else
                return GERMAN[ordinal];
        }
        else
            return GERMAN[ordinal];
    };
    // Dutch
    Ordinal.GetDutchShort = function (ordinal, plural, gender) {
        return ordinal.toString() + "e";
    };
    Ordinal.GetDutchLong = function (ordinal, plural, gender) {
        var DUTCH = [
            "",
            "eerste",
            "tweede",
            "derde",
            "vierde",
            "vijfde",
            "zesde",
            "zevende",
            "achtste",
            "negende",
            "tiende"
        ];
        return DUTCH[ordinal];
    };
    // French
    Ordinal.GetFrenchShort = function (ordinal, plural, gender) {
        var str;
        if (ordinal == 1) {
            if (gender == language_1.Gender.Male)
                str = "er";
            else
                str = "re";
        }
        else
            str = "e";
        return ordinal.toString() + str;
    };
    Ordinal.GetFrenchLong = function (ordinal, plural, gender) {
        var FRENCH = [
            "",
            "premier",
            "deuxième",
            "troisième",
            "quatrième",
            "cinquième",
            "sixième",
            "septième",
            "huitième",
            "neuvième",
            "dixième"
        ];
        if (ordinal == SmallOrdinal.First) {
            if (gender == language_1.Gender.Male)
                return "premier";
            else
                return "première";
        }
        else
            return FRENCH[ordinal];
    };
    // Finnish
    Ordinal.GetFinnishLong = function (ordinal, plural, gender) {
        var FINNISH_SINGULARS = [
            "",
            "ensimmäinen",
            "toinen",
            "kolmas",
            "neljäs",
            "viides",
            "kuudes",
            "seitsemäs",
            "kahdeksas",
            "yhdeksäs",
            "kymmenes"
        ];
        var FINNISH_PLURALS = [
            "",
            "ensimmäiset",
            "toiset",
            "kolmannet",
            "neljännet",
            "viidennet",
            "kuudennet",
            "seitsennet",
            "kahdeksannet",
            "yhdeksännet",
            "kymmenennet"
        ];
        if (plural == language_1.PluralForm.One)
            return FINNISH_SINGULARS[ordinal];
        else
            return FINNISH_PLURALS[ordinal];
    };
    // Estonian, TODO: plural forms
    Ordinal.GetEstonianLong = function (ordinal, plural, gender) {
        var ESTONIAN_PLURALS = [
            "",
            "esimene",
            "teine",
            "kolmas",
            "neljas",
            "viies",
            "kuues",
            "seitsmes",
            "kaheksas",
            "üheksas",
            "kümnes"
        ];
        return ESTONIAN_PLURALS[ordinal];
    };
    // Danish
    Ordinal.GetDanishLong = function (ordinal, plural, gender) {
        var DANISH = [
            "",
            "første",
            "anden",
            "tredje",
            "fjerde",
            "femte",
            "sjette",
            "syvende",
            "ottende",
            "niende",
            "tiende"
        ];
        return DANISH[ordinal];
    };
    // Swedish
    Ordinal.GetSwedishLong = function (ordinal, plural, gender) {
        var SWEDISH = [
            "",
            "första",
            "andra",
            "tredje",
            "fjärde",
            "femte",
            "sjätte",
            "sjunde",
            "åttonde",
            "nionde",
            "tionde"
        ];
        return SWEDISH[ordinal];
    };
    // Norwegian, Bokmål
    Ordinal.GetNorwegianBokmalLong = function (ordinal, plural, gender) {
        var NORWEGIAN_BOKMAL = [
            "",
            "første",
            "annen",
            "tredje",
            "fjerde",
            "femte",
            "sjette",
            "sjuende",
            "åttende",
            "niende",
            "tiende"
        ];
        return NORWEGIAN_BOKMAL[ordinal];
    };
    // Norwegian, Nynorsk
    Ordinal.GetNorwegianNynorskLong = function (ordinal, plural, gender) {
        var NORWEGIAN_NYNORSK = [
            "",
            "første",
            "andre",
            "tredje",
            "fjerde",
            "femte",
            "sjette",
            "sjuande",
            "åttande",
            "niande",
            "tiande"
        ];
        return NORWEGIAN_NYNORSK[ordinal];
    };
    // Icelandic
    Ordinal.GetIcelandicLong = function (ordinal, plural, gender) {
        var ICELANDIC = [
            "",
            "fyrsti",
            "annar",
            "þriðji",
            "fjórði",
            "fimmti",
            "sjötti",
            "sjöundi",
            "áttundi",
            "níundi",
            "tíundi"
        ];
        return ICELANDIC[ordinal];
    };
    // Japanese
    Ordinal.GetJapaneseLong = function (ordinal, plural, gender) {
        var JAPANESE = [
            "",
            "一つ目",
            "二つ目",
            "三つ目",
            "四つ目",
            "五つ目",
            "六つ目",
            "七つ目",
            "八つ目",
            "九つ目",
            "十"
        ];
        return JAPANESE[ordinal];
    };
    Ordinal.GetJapaneseShort = function (ordinal, plural, gender) {
        if (this.IsShortOrdinal(ordinal))
            return this.GetJapaneseLong(ordinal, plural, gender);
        else
            return ordinal.toString() + "目";
    };
    // Korean
    Ordinal.GetKoreanLong = function (ordinal, plural, gender) {
        var KOREAN = [
            "",
            "첫째",
            "둘째",
            "셋째",
            "넷째",
            "다섯째",
            "여섯째",
            "일곱째",
            "여덟째",
            "아홉째",
            "열째"
        ];
        return KOREAN[ordinal];
    };
    Ordinal.GetKoreanShort = function (ordinal, plural, gender) {
        if (this.IsShortOrdinal(ordinal))
            return this.GetKoreanLong(ordinal, plural, gender);
        else
            return ordinal.toString() + "째";
    };
    // Simplified Chinese
    Ordinal.GetSimplifiedChineseLong = function (ordinal, plural, gender) {
        var SIMPLIFIED_CHINESE = [
            "",
            "第一",
            "第二",
            "第三",
            "第四",
            "第五",
            "第六",
            "第七",
            "第八",
            "第九",
            "第十"
        ];
        return SIMPLIFIED_CHINESE[ordinal];
    };
    Ordinal.GetSimplifiedChineseShort = function (ordinal, plural, gender) {
        if (this.IsShortOrdinal(ordinal))
            return this.GetSimplifiedChineseLong(ordinal, plural, gender);
        else
            return "第" + ordinal.toString();
    };
    // Static initializer that is called automatically. Register ordinal functions.
    Ordinal.initialize = function () {
        if (this.datas[""] != null)
            return;
        this.Register("", this.GetDefaultLong, this.GetDefaultShort);
        this.Register("en", this.GetEnglishLong, this.GetEnglishShort);
        this.Register("de", this.GetGermanLong, this.GetPeriodShort);
        this.Register("nl", this.GetDutchLong, this.GetDutchShort);
        this.Register("fr", this.GetFrenchLong, this.GetFrenchShort);
        this.Register("fi", this.GetFinnishLong, this.GetPeriodShort);
        this.Register("et", this.GetEstonianLong, this.GetPeriodShort);
        this.Register("sv", this.GetSwedishLong, this.GetPeriodShort);
        this.Register("da", this.GetDanishLong, this.GetPeriodShort);
        this.Register("no", this.GetNorwegianBokmalLong, this.GetPeriodShort);
        this.Register("nb", this.GetNorwegianBokmalLong, this.GetPeriodShort);
        this.Register("nn", this.GetNorwegianNynorskLong, this.GetPeriodShort);
        this.Register("is", this.GetIcelandicLong, this.GetPeriodShort);
        this.Register("ja", this.GetJapaneseShort, this.GetJapaneseLong);
        this.Register("ko", this.GetKoreanShort, this.GetKoreanLong);
        this.Register("zh", this.GetSimplifiedChineseLong, this.GetSimplifiedChineseShort);
        this.Register("zh-Hans", this.GetSimplifiedChineseLong, this.GetSimplifiedChineseShort);
    };
    Ordinal.datas = {};
    Ordinal = __decorate([
        initialize
    ], Ordinal);
    return Ordinal;
}(language_1.Language));
exports.Ordinal = Ordinal;
//# sourceMappingURL=ordinal.js.map