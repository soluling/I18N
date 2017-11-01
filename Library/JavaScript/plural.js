var Plural;
(function (Plural) {
    var Sample = (function () {
        function Sample() {
        }
        Sample.Print = function () {
        };
        return Sample;
    })();
    Plural.Sample = Sample;

    var PluralKind;
    (function (PluralKind) {
        PluralKind[PluralKind["Default"] = 0] = "Default";
        PluralKind[PluralKind["Arabic"] = 1] = "Arabic";
        PluralKind[PluralKind["Czech"] = 2] = "Czech";
        PluralKind[PluralKind["French"] = 3] = "French";
        PluralKind[PluralKind["Icelandic"] = 4] = "Icelandic";
        PluralKind[PluralKind["Irish"] = 5] = "Irish";
        PluralKind[PluralKind["Japanese"] = 6] = "Japanese";
        PluralKind[PluralKind["Latvian"] = 7] = "Latvian";
        PluralKind[PluralKind["Lithuanian"] = 8] = "Lithuanian";
        PluralKind[PluralKind["Macedonian"] = 9] = "Macedonian";
        PluralKind[PluralKind["Maltese"] = 10] = "Maltese";
        PluralKind[PluralKind["Polish"] = 11] = "Polish";
        PluralKind[PluralKind["Romanian"] = 12] = "Romanian";
        PluralKind[PluralKind["Russian"] = 13] = "Russian";
        PluralKind[PluralKind["Slovenian"] = 14] = "Slovenian";
        PluralKind[PluralKind["Welsh"] = 15] = "Welsh";
    })(PluralKind || (PluralKind = {}));
    ;

    var indexFunction = null;
    var locale = "en";

    function GetDefaultIndex(count) {
        if (count == 1)
            return 0;
else
            return 1;
    }

    function GetArabicIndex(count) {
        if (count == 0)
            return 0;
else if (count == 1)
            return 1;
else if (count == 2)
            return 2;
else if ((count % 100 >= 3) && (count % 100 <= 10))
            return 3;
else if ((count % 100 >= 11) && (count % 100 <= 99))
            return 4;
else
            return 5;
    }

    function GetCzechIndex(count) {
        if (count == 1)
            return 0;
else if ((count >= 2) && (count <= 4))
            return 1;
else
            return 2;
    }

    function GetFrenchIndex(count) {
        if (count <= 1)
            return 0;
else
            return 1;
    }

    function GetIcelandicIndex(count) {
        if ((count % 10 != 1) || (count % 100 == 11))
            return 1;
else
            return 0;
    }

    function GetIrishIndex(count) {
        if (count == 1)
            return 0;
else if (count == 2)
            return 1;
else
            return 2;
    }

    function GetJapaneseIndex(count) {
        return 0;
    }

    function GetLatvianIndex(count) {
        if ((count % 10 == 1) && (count % 100 != 11))
            return 0;
else if (count != 0)
            return 1;
else
            return 2;
    }

    function GetLithuanianIndex(count) {
        if ((count % 10 == 1) && (count % 100 != 11))
            return 0;
else if ((count % 10 == 2) && (count % 100 != 12))
            return 1;
else
            return 2;
    }

    function GetMacedonianIndex(count) {
        if (count % 10 == 1)
            return 0;
else if (count % 10 == 2)
            return 1;
else
            return 2;
    }

    function GetMalteseIndex(count) {
        if (count == 1)
            return 0;
else if ((count == 0) || ((count % 100 >= 2) && (count % 100 <= 10)))
            return 1;
else if ((count % 100 >= 11) && (count % 100 < 20))
            return 2;
else
            return 3;
    }

    function GetPolishIndex(count) {
        if (count == 1)
            return 0;
else if ((count % 10 >= 2) && (count % 10 <= 4) && (((count % 100 < 10) || (count % 100 > 20))))
            return 1;
else
            return 2;
    }

    function GetRomanianIndex(count) {
        if (count == 1)
            return 0;
else if ((count == 0) || ((count % 100 >= 1) && (count % 100 <= 20)))
            return 1;
else
            return 2;
    }

    function GetRussianIndex(count) {
        if ((count % 10 == 1) && (count % 100 != 11))
            return 0;
else if ((count % 10 >= 2) && (count % 10 <= 4) && ((count % 100 < 10) || ((count % 100) > 20)))
            return 1;
else
            return 2;
    }

    function GetSlovenianIndex(count) {
        if (count % 100 == 1)
            return 0;
else if (count % 100 == 2)
            return 1;
else if ((count % 100 == 3) || (count % 100 == 4))
            return 2;
else
            return 3;
    }

    function GetWelshIndex(count) {
        if (count == 1)
            return 0;
else if (count == 2)
            return 1;
else if ((count != 8) && (count != 11))
            return 2;
else
            return 3;
    }

    function isLanguageInArray(language, array) {
        for (var thisLanguage in array)
            if (thisLanguage == language)
                return true;

        return false;
    }

    var englishKind = [
        "en",
        "de",
        "nl",
        "sv",
        "dk",
        "no",
        "nb",
        "nn",
        "fi",
        "et",
        "fo",
        "es",
        "pt",
        "it",
        "bg",
        "el",
        "eo",
        "hu",
        "he",
        "ab",
        "aa",
        "af",
        "sq",
        "an",
        "as",
        "az",
        "ba",
        "eu",
        "bn",
        "my",
        "km",
        "ca",
        "tzm",
        "co",
        "fy",
        "ka",
        "kl",
        "gu",
        "ha",
        "haw",
        "he",
        "hi",
        "kn",
        "ml",
        "mr",
        "mn",
        "ne",
        "se",
        "pa",
        "rm",
        "so",
        "sw",
        "tg",
        "te",
        "ta",
        "ur",
        "uz"
    ];

    var frenchKind = ["fr", "ak", "am", "br", "fil", "oc"];
    var japaneseKind = ["ja", "ko", "zh", "vi", "hy", "ay", "chr", "id", "ms", "fa", "syr", "th", "bo", "tr"];
    var russianKind = ["ru", "be", "uk", "sr", "hr", "bs"];
    var czechKind = ["cs", "sk"];
    var irishKind = ["ga", "gd", "dv", "iu"];

    var PROCS = [
        GetDefaultIndex,
        GetArabicIndex,
        GetCzechIndex,
        GetFrenchIndex,
        GetIcelandicIndex,
        GetIrishIndex,
        GetJapaneseIndex,
        GetLatvianIndex,
        GetLithuanianIndex,
        GetMacedonianIndex,
        GetMalteseIndex,
        GetPolishIndex,
        GetRomanianIndex,
        GetRussianIndex,
        GetSlovenianIndex,
        GetWelshIndex
    ];

    function GetKind(locale) {
        var language = "en";
        var country = "";
        var index = locale.indexOf("-");

        if (index > 0) {
            language = locale.substr(0, index);
            country = locale.substr(index + 1, locale.length);
        } else {
            language = locale;
            country = "";
        }

        if (isLanguageInArray(language, frenchKind) || ((language == "pt") && (country == "BR")))
            return PluralKind.French;
else if (isLanguageInArray(language, englishKind))
            return PluralKind.Default;
else if (isLanguageInArray(language, japaneseKind))
            return PluralKind.Japanese;
else if (isLanguageInArray(language, russianKind))
            return PluralKind.Russian;
else if (isLanguageInArray(language, czechKind))
            return PluralKind.Czech;
else if (isLanguageInArray(language, irishKind))
            return PluralKind.Irish;
else if (language == "ar")
            return PluralKind.Arabic;
else if (language == "is")
            return PluralKind.Icelandic;
else if (language == "lv")
            return PluralKind.Latvian;
else if (language == "lt")
            return PluralKind.Lithuanian;
else if (language == "mk")
            return PluralKind.Macedonian;
else if (language == "mt")
            return PluralKind.Maltese;
else if (language == "pl")
            return PluralKind.Polish;
else if (language == "ro")
            return PluralKind.Romanian;
else if (language == "sl")
            return PluralKind.Slovenian;
else if (language == "cy")
            return PluralKind.Welsh;
else
            return PluralKind.Default;
    }

    function GetIndexProc(locale) {
        return PROCS[GetKind(locale)];
    }
    Plural.GetIndexProc = GetIndexProc;

    function ParsePattern(pattern) {
        var items = new Array();
        var i = 0;

        while (i < pattern.length) {
            var str = "";

            while (i < pattern.length) {
                var c = pattern.charAt(i);

                if (c == ';') {
                    i++;
                    break;
                } else if ((c == '\\') && (i < pattern.length - 1)) {
                    i++;
                    c = pattern.charAt(i);

                    if (c == ';')
                        c = ';';
                }

                str = str + c;
                i++;
            }

            items.push(str);
        }

        return items;
    }

    function getPattern(format, count) {
        if (indexFunction == null)
            indexFunction = GetIndexProc(locale);

        var index = indexFunction(count);
        var parts = ParsePattern(format);

        if (index < parts.length)
            return parts[index];
else
            return parts[parts.length - 1];
    }
    Plural.getPattern = getPattern;

    function setPluralLanguage(value) {
        locale = value;
    }
    Plural.setPluralLanguage = setPluralLanguage;

    function pluralSprintf(format, count, arg) {
        return sprintf(getPattern(format, count), arg);
    }
    Plural.pluralSprintf = pluralSprintf;
})(Plural || (Plural = {}));
//# sourceMappingURL=plural.js.map
