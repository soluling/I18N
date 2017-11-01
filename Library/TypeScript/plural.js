"use strict";
var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
var language_1 = require("./language");
/**
 * Enumeration that specifies what plural group where language belongs to.
 * The algorithm is named based on the main language of the group.
 * For example PluralGroup.Japanese is not only applied to Japanese but all the other languages where plural rules are the same as in Japanese (expecially Asian ones).
 */
(function (PluralGroup) {
    PluralGroup[PluralGroup["Default"] = 0] = "Default";
    PluralGroup[PluralGroup["Arabic"] = 1] = "Arabic";
    PluralGroup[PluralGroup["Czech"] = 2] = "Czech";
    PluralGroup[PluralGroup["French"] = 3] = "French";
    PluralGroup[PluralGroup["Icelandic"] = 4] = "Icelandic";
    PluralGroup[PluralGroup["Irish"] = 5] = "Irish";
    PluralGroup[PluralGroup["Japanese"] = 6] = "Japanese";
    PluralGroup[PluralGroup["Latvian"] = 7] = "Latvian";
    PluralGroup[PluralGroup["Lithuanian"] = 8] = "Lithuanian";
    PluralGroup[PluralGroup["Macedonian"] = 9] = "Macedonian";
    PluralGroup[PluralGroup["Maltese"] = 10] = "Maltese";
    PluralGroup[PluralGroup["Polish"] = 11] = "Polish";
    PluralGroup[PluralGroup["Romanian"] = 12] = "Romanian";
    PluralGroup[PluralGroup["Russian"] = 13] = "Russian";
    PluralGroup[PluralGroup["Slovenian"] = 14] = "Slovenian";
    PluralGroup[PluralGroup["Welsh"] = 15] = "Welsh"; // Language group that behaves like Welsh.
})(exports.PluralGroup || (exports.PluralGroup = {}));
var PluralGroup = exports.PluralGroup;
;
var Pattern = (function () {
    /**
     * Create a pattern
     */
    function Pattern(value) {
        this.Value = value;
    }
    return Pattern;
}());
/***
 * Specifies a single plural pattern. It contains the pattern string and the plural form where it is used.
 */
var PluralPattern = (function (_super) {
    __extends(PluralPattern, _super);
    /**
     * Create a plural pattern.
     * @param {string} value - The pattern string.
     * @param {PluralForm} form - The plural form where this pattern is used.
     */
    function PluralPattern(value, form) {
        _super.call(this, value);
        this.Form = form;
    }
    return PluralPattern;
}(Pattern));
/***
 * Specifies a single gender pattern. It contains the pattern string and the gender value where it is used.
 */
var GenderPattern = (function (_super) {
    __extends(GenderPattern, _super);
    /**
     * Create a gender pattern.
     * @param {string} value - The pattern string.
     * @param {Gender} form - The gender when this pattern is used.
     */
    function GenderPattern(value, gender) {
        _super.call(this, value);
        this.Gender = gender;
    }
    return GenderPattern;
}(Pattern));
/**
 * Part of the message pattern. Generally contains one plural or gender placeholder. Contain one or more patterns - one for each pluar or gender forms.
 */
var FormatPart = (function () {
    function FormatPart() {
        this.items = [];
    }
    Object.defineProperty(FormatPart.prototype, "Count", {
        /**
         * Get the pattern count.
         * @return {number} The pattern count.
         */
        get: function () {
            return this.items.length;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(FormatPart.prototype, "OtherValue", {
        /**
         * Get the pattern for Plural.Other.
         * @return {string} The other pattern.
         */
        get: function () {
            return this.GetPluralString(language_1.PluralForm.Other);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(FormatPart.prototype, "DefaultValue", {
        /**
         * Get the first pattern.
         * @return {string} The first pattern.
         */
        get: function () {
            if (this.Count > 0)
                return this.GetPattern(0).Value;
            else
                return "";
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(FormatPart.prototype, "IsPlural", {
        /**
         * Check if the part is a plural part.
         * @return {boolean} Return true if this part is a plural part.
         */
        get: function () {
            for (var i = 0; i < this.Count; i++) {
                var pattern = this.GetPattern(i);
                if (pattern instanceof PluralPattern)
                    return true;
            }
            return false;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(FormatPart.prototype, "IsGender", {
        /**
         * Check if the part is a gender part.
         * @return {boolean} Return true if this part is a gender part.
         */
        get: function () {
            for (var i = 0; i < this.Count; i++) {
                var pattern = this.GetPattern(i);
                if (pattern instanceof GenderPattern)
                    return true;
            }
            return false;
        },
        enumerable: true,
        configurable: true
    });
    /**
     * Get a pattern.
     * @param {number} index The index of the pattern
     * @return {Pattern} The pattern.
     */
    FormatPart.prototype.GetPattern = function (index) {
        return this.items[index];
    };
    /**
     * Get a pattern string.
     * @param {PluralForm} form The plural form that is used.
     * @param {number} count The count where the pattern will be used.
     * @return {string} The pattern string.
     */
    FormatPart.prototype.GetCheckedPluralString = function (form, count) {
        if (!this.IsPlural)
            throw new Error("Pattern is not a plural pattern");
        var pattern;
        if ((count == 0) && this.PluralExists(language_1.PluralForm.Zero))
            pattern = this.GetPluralString(language_1.PluralForm.Zero);
        else if ((form == language_1.PluralForm.Other) && (count == 1) && this.PluralExists(language_1.PluralForm.One))
            pattern = this.GetPluralString(language_1.PluralForm.One);
        else if ((form == language_1.PluralForm.Other) && (count == 2) && this.PluralExists(language_1.PluralForm.Two))
            pattern = this.GetPluralString(language_1.PluralForm.Two);
        else
            pattern = this.GetPluralString(form);
        if (pattern == "")
            pattern = this.DefaultValue;
        return pattern;
    };
    FormatPart.prototype.GetCheckedGenderString = function (gender) {
        if (!this.IsGender)
            throw new Error("Pattern is not a gender pattern");
        var pattern = this.GetGenderString(gender);
        if (pattern == "")
            pattern = this.DefaultValue;
        return pattern;
    };
    FormatPart.prototype.GetPluralString = function (form) {
        var item = this.FindPlural(form);
        if (item != null)
            return item.Value;
        else
            return "";
    };
    FormatPart.prototype.SetPluralString = function (form, value) {
        var item = this.FindPlural(form);
        if (item != null)
            item.Value = value;
        else
            this.AddPlural(value, form);
    };
    FormatPart.prototype.GetGenderString = function (gender) {
        var item = this.FindGender(gender);
        if (item != null)
            return item.Value;
        else
            return "";
    };
    FormatPart.prototype.SetGenderString = function (gender, value) {
        var item = this.FindGender(gender);
        if (item != null)
            item.Value = value;
        else
            this.AddGender(value, gender);
    };
    FormatPart.prototype.FindPlural = function (form) {
        for (var _i = 0, _a = this.items; _i < _a.length; _i++) {
            var pattern = _a[_i];
            if ((pattern instanceof PluralPattern) && (pattern.Form == form))
                return pattern;
        }
        return null;
    };
    FormatPart.prototype.FindGender = function (gender) {
        for (var _i = 0, _a = this.items; _i < _a.length; _i++) {
            var pattern = _a[_i];
            if ((pattern instanceof GenderPattern) && (pattern.Gender == gender))
                return pattern;
        }
        return null;
    };
    FormatPart.prototype.PluralExists = function (form) {
        return this.FindPlural(form) != null;
    };
    FormatPart.prototype.GenderExists = function (gender) {
        return this.FindGender(gender) != null;
    };
    FormatPart.prototype.AddPlural = function (value, form) {
        var item = new PluralPattern(value, form);
        this.items.push(item);
        return item;
    };
    FormatPart.prototype.AddGender = function (value, gender) {
        var item = new GenderPattern(value, gender);
        this.items.push(item);
        return item;
    };
    return FormatPart;
}());
var NEXT = "next";
/**
 *
 */
var FormatString = (function () {
    function FormatString(pattern) {
        this.items = [];
        this.StartPattern = "";
        this.ParsePatterns(pattern);
    }
    Object.defineProperty(FormatString.prototype, "Count", {
        get: function () {
            return this.items.length;
        },
        enumerable: true,
        configurable: true
    });
    FormatString.prototype.GetItem = function (index) {
        return this.items[index];
    };
    FormatString.prototype.Find = function (form, all) {
        if (all === void 0) { all = true; }
        var result;
        if (all) {
            for (var i = 0; i < this.Count; i++) {
                result = this.GetItem(i).FindPlural(form);
                if (result != null)
                    return result;
            }
            return null;
        }
        else
            return this.GetItem(0).FindPlural(form);
    };
    FormatString.prototype.AddParameter = function () {
        var result = new FormatPart();
        this.items.push(result);
        return result;
    };
    FormatString.prototype.ParsePatterns = function (patterns) {
        var formStr = "";
        var genderStr = "";
        var parameter = this.AddParameter();
        var items = new Array();
        var i = 0;
        while (i < patterns.length) {
            var str = "";
            while (i < patterns.length) {
                var c = patterns.charAt(i);
                if (c == ';') {
                    i++;
                    if ((i >= patterns.length) || (patterns.charAt(i) != ';'))
                        break;
                }
                str = str + c;
                i++;
            }
            if (str == NEXT) {
                // New parameter
                parameter = this.AddParameter();
            }
            else if (formStr != "") {
                // Add plural pattern
                parameter.AddPlural(str, Plural.StringToPlural(formStr));
                formStr = "";
            }
            else if (genderStr != "") {
                // Add gender pattern
                parameter.AddGender(str, Plural.StringToGender(genderStr));
                genderStr = "";
            }
            else if (Plural.IsPluralForm(str)) {
                // Plural pattern follows
                formStr = str;
            }
            else if (Plural.IsGender(str)) {
                // Gender pattern follows
                genderStr = str;
            }
            else if (this.StartPattern == "")
                this.StartPattern = str;
            else
                throw new Error("Invalid pattern: " + patterns);
        }
    };
    return FormatString;
}());
exports.FormatString = FormatString;
var FormData = (function () {
    function FormData() {
    }
    return FormData;
}());
/**
 * Implements a plural enabled string format functions.
 */
var Plural = (function (_super) {
    __extends(Plural, _super);
    function Plural() {
        _super.apply(this, arguments);
    }
    Plural.GetDefaultIndex = function (count) {
        if (count == 1)
            return language_1.PluralForm.One;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetArabicIndex = function (count) {
        if (count == 0)
            return language_1.PluralForm.Zero;
        else if (count == 1)
            return language_1.PluralForm.One;
        else if (count == 2)
            return language_1.PluralForm.Two;
        else if ((count % 100 >= 3) && (count % 100 <= 10))
            return language_1.PluralForm.Few;
        else if ((count % 100 >= 11) && (count % 100 <= 99))
            return language_1.PluralForm.Many;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetCzechIndex = function (count) {
        if (count == 1)
            return language_1.PluralForm.One;
        else if ((count >= 2) && (count <= 4))
            return language_1.PluralForm.Few;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetFrenchIndex = function (count) {
        if (count <= 1)
            return language_1.PluralForm.One;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetIcelandicIndex = function (count) {
        if ((count % 10 != 1) || (count % 100 == 11))
            return language_1.PluralForm.Other;
        else
            return language_1.PluralForm.One;
    };
    Plural.GetIrishIndex = function (count) {
        if (count == 1)
            return language_1.PluralForm.One;
        else if (count == 2)
            return language_1.PluralForm.Two;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetJapaneseIndex = function (count) {
        return language_1.PluralForm.Other;
    };
    Plural.GetLatvianIndex = function (count) {
        if ((count % 10 == 1) && (count % 100 != 11))
            return language_1.PluralForm.One;
        else if (count != 0)
            return language_1.PluralForm.Zero;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetLithuanianIndex = function (count) {
        if ((count % 10 == 1) && (count % 100 != 11))
            return language_1.PluralForm.One;
        else if ((count % 10 == 2) && (count % 100 != 12))
            return language_1.PluralForm.Few;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetMacedonianIndex = function (count) {
        if (count % 10 == 1)
            return language_1.PluralForm.One;
        else if (count % 10 == 2)
            return language_1.PluralForm.Two;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetMalteseIndex = function (count) {
        if (count == 1)
            return language_1.PluralForm.One;
        else if ((count == 0) || ((count % 100 >= 2) && (count % 100 <= 10)))
            return language_1.PluralForm.Few;
        else if ((count % 100 >= 11) && (count % 100 < 20))
            return language_1.PluralForm.Many;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetPolishIndex = function (count) {
        if (count == 1)
            return language_1.PluralForm.One;
        else if ((count % 10 >= 2) && (count % 10 <= 4) && (((count % 100 < 10) || (count % 100 > 20))))
            return language_1.PluralForm.Few;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetRomanianIndex = function (count) {
        if (count == 1)
            return language_1.PluralForm.One;
        else if ((count == 0) || ((count % 100 >= 1) && (count % 100 <= 20)))
            return language_1.PluralForm.Few;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetRussianIndex = function (count) {
        if ((count % 10 == 1) && (count % 100 != 11))
            return language_1.PluralForm.One;
        else if ((count % 10 >= 2) && (count % 10 <= 4) && ((count % 100 < 10) || ((count % 100) > 20)))
            return language_1.PluralForm.Few;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetSlovenianIndex = function (count) {
        if (count % 100 == 1)
            return language_1.PluralForm.One;
        else if (count % 100 == 2)
            return language_1.PluralForm.Two;
        else if ((count % 100 == 3) || (count % 100 == 4))
            return language_1.PluralForm.Few;
        else
            return language_1.PluralForm.Other;
    };
    Plural.GetWelshIndex = function (count) {
        if (count == 0)
            return language_1.PluralForm.Zero;
        else if (count == 1)
            return language_1.PluralForm.One;
        else if (count == 2)
            return language_1.PluralForm.Two;
        else if (count == 3)
            return language_1.PluralForm.Few;
        else if (count == 6)
            return language_1.PluralForm.Many;
        else
            return language_1.PluralForm.Other;
    };
    Plural.DoStringToPlural = function (value) {
        for (var i = 0; i < this.FORM_DATAS.length; i++) {
            var formData = this.FORM_DATAS[i];
            if ((value == formData.id) || ((formData.num >= 0) && ((value == formData.num.toString()) || (value == "=" + formData.num.toString()))))
                return [true, i];
        }
        return [false, language_1.PluralForm.Other];
    };
    /**
     * Convert plural form code into a plural code.
     * @param {string} value The plural form code such as "one".
     * @return The plural code.
     */
    Plural.StringToPlural = function (value) {
        return this.DoStringToPlural(value)[1];
    };
    /**
     * Check if the passed string is a plural form code.
     * @param {string} value The plural form code to be checked such as "one".
     * @return True if the passed string is a valid plural form code.
     */
    Plural.IsPluralForm = function (value) {
        return this.DoStringToPlural(value)[0];
    };
    Plural.DoStringToGender = function (value) {
        for (var i = 0; i < this.GENDER_DATAS.length; i++) {
            if (value == this.GENDER_DATAS[i])
                return [true, i];
        }
        return [false, language_1.Gender.Neutral];
    };
    /**
     * Convert gender code into a gender.
     * @param {string} value The gender code such as "male".
     * @return The gender.
     */
    Plural.StringToGender = function (value) {
        return this.DoStringToGender(value)[1];
    };
    /**
     * Check if the passed string is a gender code.
     * @param {string} value The gender code to be checked such as "male".
     * @return True if the passed string is a valid gender code.
     */
    Plural.IsGender = function (value) {
        return this.DoStringToGender(value)[0];
    };
    Plural.IsLanguageInArray = function (language, array) {
        for (var _i = 0, array_1 = array; _i < array_1.length; _i++) {
            var thisLanguage = array_1[_i];
            if (thisLanguage == language)
                return true;
        }
        return false;
    };
    Plural.GetKind = function (locale) {
        var language = "en";
        var country = "";
        var ids = locale.split("-");
        language = ids[0];
        if (ids.length > 1)
            country = ids[1];
        if (this.IsLanguageInArray(language, this.frenchKind) || ((language == "pt") && (country == "BR")))
            return PluralGroup.French;
        else if (this.IsLanguageInArray(language, this.englishKind))
            return PluralGroup.Default;
        else if (this.IsLanguageInArray(language, this.japaneseKind))
            return PluralGroup.Japanese;
        else if (this.IsLanguageInArray(language, this.russianKind))
            return PluralGroup.Russian;
        else if (this.IsLanguageInArray(language, this.czechKind))
            return PluralGroup.Czech;
        else if (this.IsLanguageInArray(language, this.irishKind))
            return PluralGroup.Irish;
        else if (language == "ar")
            return PluralGroup.Arabic;
        else if (language == "is")
            return PluralGroup.Icelandic;
        else if (language == "lv")
            return PluralGroup.Latvian;
        else if (language == "lt")
            return PluralGroup.Lithuanian;
        else if (language == "mk")
            return PluralGroup.Macedonian;
        else if (language == "mt")
            return PluralGroup.Maltese;
        else if (language == "pl")
            return PluralGroup.Polish;
        else if (language == "ro")
            return PluralGroup.Romanian;
        else if (language == "sl")
            return PluralGroup.Slovenian;
        else if (language == "cy")
            return PluralGroup.Welsh;
        else
            return PluralGroup.Default;
    };
    /**
     * Get the plural index function.
     * @param {string} locale The locale whose plural index function to get.
     * @return {any} The plural index function.
     */
    Plural.GetIndexProc = function (locale) {
        return Plural.PROCS[this.GetKind(locale)];
    };
    /**
     * Initialize the plural index function for the active language if not already initialized.
     */
    Plural.InitializeIndexProc = function () {
        if (Plural.indexProc == null)
            Plural.indexProc = this.GetIndexProc(this.locale);
    };
    /**
     * Get the pattern matching the specified count.
     * @param {string} patterns The multi pattern string.
     * @param {number} count The count.
     * @return {string} The pattern.
     */
    Plural.GetPluralPattern = function (patterns, count) {
        this.InitializeIndexProc();
        var form = Plural.indexProc(count);
        return new FormatString(patterns).GetItem(0).GetCheckedPluralString(form, count);
    };
    /**
     * Get the pattern matching the specified gender.
     * @param {string} patterns The multi pattern string.
     * @param {Gender} gender The gender.
     * @return {string} The pattern.
     */
    Plural.GetGenderPattern = function (patterns, gender) {
        return new FormatString(patterns).GetItem(0).GetCheckedGenderString(gender);
    };
    /**
     * Interpolates the pattern with the parameters.
     * @param {string} pattern - The pattern string.
     * @param {any} data - Arguments for the interpolation pattern. Either single object or an array of objects.
     * @return {string} The result string where parameter(s) were interpolated with the pattern.
     */
    Plural.Interpolate = function (pattern, data) {
        return pattern.replace(/\{\{ *([\w_]+) *\}\}/g, function (str, key) {
            var value;
            if (Array.isArray(data)) {
                var args = data;
                for (var _i = 0, args_1 = args; _i < args_1.length; _i++) {
                    var arg = args_1[_i];
                    value = arg[key];
                    if (value != undefined)
                        break;
                }
                ;
            }
            else {
                value = data[key];
            }
            if (value === undefined)
                throw new Error('No value provided for variable ' + str);
            else if (typeof value === 'function')
                value = value(data);
            return value;
        });
    };
    /**
     * Selects the right pattern from the passed multi pattern string and applies that for interpolation.
     * @param {string} patterns - The multi pattern string.
     * @param {any} args - Arguments for the interpolation pattern.
     * @return {string} The result string where parameter(s) were interpolated with the pattern.
     */
    Plural.FormatPlural = function (patterns, args) {
        var count = 1;
        if (typeof args == "number")
            count = args;
        else {
            for (var argKey in args) {
                count = args[argKey];
                break;
            }
        }
        var pattern = this.GetPluralPattern(patterns, count);
        return this.Interpolate(pattern, args);
    };
    /**
     * Selects the right gender pattern from the passed multi pattern string and applies that for interpolation.
     * @param {string} patterns - The multi pattern string.
     * @param {Gender} gender - Gender value.
     * @param {any} args - Optional arguments for interpolation.
     * @return {string} The result string where parameter(s) were interpolated with the pattern.
     */
    Plural.FormatGender = function (patterns, gender) {
        var args = [];
        for (var _i = 2; _i < arguments.length; _i++) {
            args[_i - 2] = arguments[_i];
        }
        var pattern = this.GetGenderPattern(patterns, gender);
        return this.Interpolate(pattern, args);
    };
    /**
     * Selects the right gender pattern from the passed multi pattern string and applies that for interpolation.
     * @param {string} patterns - The multi pattern string.
     * @param {any} args - Arguments for interpolation. For plural parameter pass an object having a single number property. For gender parameter pass an object containing gender and value properties.
     * @return {string} The result string where parameter(s) were interpolated with the pattern.
     */
    Plural.Format = function (patterns) {
        var args = [];
        for (var _i = 1; _i < arguments.length; _i++) {
            args[_i - 1] = arguments[_i];
        }
        this.InitializeIndexProc();
        var result = "";
        var str = new FormatString(patterns);
        var count = args.length;
        if (count != str.Count)
            throw new Error("Pattern " + patterns + " does not have enough patterns");
        // Process parameter from last to first and build the result string part by part
        for (var i = str.Count - 1; i >= 0; i--) {
            var parameter = str.GetItem(i);
            var arg = args[i];
            var keys = Object.keys(arg);
            var pattern;
            if (arg["gender"] != undefined) {
                // Gender
                var gender = arg["gender"];
                pattern = parameter.GetCheckedGenderString(gender);
            }
            else if (keys.length > 0) {
                // Plural
                var count = arg[keys[0]];
                var form = Plural.indexProc(count);
                pattern = parameter.GetCheckedPluralString(form, count);
            }
            else
                throw new Error("No parameters for part #" + i);
            if (i < str.Count - 1)
                arg["next"] = result;
            result = this.Interpolate(pattern, arg);
        }
        if (str.StartPattern != "")
            result = this.Interpolate(str.StartPattern, result);
        return result;
    };
    Plural.indexProc = null;
    // Size and order must match PluralForm
    Plural.FORM_DATAS = [
        { id: "zero", num: 0 },
        { id: "one", num: 1 },
        { id: "two", num: 2 },
        { id: "few", num: -1 },
        { id: "many", num: -1 },
        { id: "other", num: -1 }
    ];
    // Size and order must match Gender
    Plural.GENDER_DATAS = [
        "male",
        "female",
        "neutral"
    ];
    // Size and order must match PluralGroup
    Plural.PROCS = [
        Plural.GetDefaultIndex,
        Plural.GetArabicIndex,
        Plural.GetCzechIndex,
        Plural.GetFrenchIndex,
        Plural.GetIcelandicIndex,
        Plural.GetIrishIndex,
        Plural.GetJapaneseIndex,
        Plural.GetLatvianIndex,
        Plural.GetLithuanianIndex,
        Plural.GetMacedonianIndex,
        Plural.GetMalteseIndex,
        Plural.GetPolishIndex,
        Plural.GetRomanianIndex,
        Plural.GetRussianIndex,
        Plural.GetSlovenianIndex,
        Plural.GetWelshIndex
    ];
    Plural.englishKind = [
        "en", "de", "nl", "sv", "dk", "no", "nb", "nn", "fi", "et", "fo", "es", "pt", "it", "bg", "el", "eo", "hu", "he",
        "ab", "aa", "af", "sq", "an", "as", "az", "ba", "eu", "bn", "my", "km", "ca", "tzm", "co", "fy", "ka", "kl", "gu",
        "ha", "haw", "he", "hi", "kn", "ml", "mr", "mn", "ne", "se", "pa", "rm", "so", "sw", "tg", "te", "ta", "ur", "uz"
    ];
    Plural.frenchKind = ["fr", "ak", "am", "br", "fil", "oc"];
    Plural.japaneseKind = ["ja", "ko", "zh", "vi", "hy", "ay", "chr", "id", "ms", "fa", "syr", "th", "bo", "tr"];
    Plural.russianKind = ["ru", "be", "uk", "sr", "hr", "bs"];
    Plural.czechKind = ["cs", "sk"];
    Plural.irishKind = ["ga", "gd", "dv", "iu"];
    return Plural;
}(language_1.Language));
exports.Plural = Plural;
//# sourceMappingURL=plural.js.map