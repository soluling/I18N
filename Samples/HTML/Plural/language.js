define(["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    /**
     * Enumeration that specifies the plural form. There are six different plural forms from singular to various plurals.
     * How many forms a language uses depends on the language. Most languages only use singular and plural.
     */
    var PluralForm;
    (function (PluralForm) {
        PluralForm[PluralForm["Zero"] = 0] = "Zero";
        PluralForm[PluralForm["One"] = 1] = "One";
        PluralForm[PluralForm["Two"] = 2] = "Two";
        PluralForm[PluralForm["Few"] = 3] = "Few";
        PluralForm[PluralForm["Many"] = 4] = "Many";
        PluralForm[PluralForm["Other"] = 5] = "Other"; // Plural. Used when count does not belong to any other group. All languages support this. Most often this is the plural form.
    })(PluralForm = exports.PluralForm || (exports.PluralForm = {}));
    ;
    /**
     * Enumeration that specifies the gender form. There are three different gender forms.
     * How many forms a language uses depends on the language. Many languages do not use gender so they only use other.
     */
    var Gender;
    (function (Gender) {
        Gender[Gender["Male"] = 0] = "Male";
        Gender[Gender["Female"] = 1] = "Female";
        Gender[Gender["Neutral"] = 2] = "Neutral"; // Other or neutral. Used when language does not use gender or gender is neutral. Most languages support this.</summary>
    })(Gender = exports.Gender || (exports.Gender = {}));
    ;
    var Language = (function () {
        function Language() {
        }
        /**
         * Set the active language.
         * @param {string} value The language or locale id.
         */
        Language.SetLanguage = function (value) {
            this.locale = value;
        };
        Language.locale = "en";
        return Language;
    }());
    exports.Language = Language;
});
//# sourceMappingURL=language.js.map