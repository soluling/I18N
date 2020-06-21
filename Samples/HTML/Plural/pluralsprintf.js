var __extends = (this && this.__extends) || (function () {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
        function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
define(["require", "exports", "./plural", "sprintf-js"], function (require, exports, plural_1, sprintf_js_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    /**
     * Implements multi pattern format using sprintf-js.
     * This class uses the same format as printf of C language. If you are more familiar to that format than the modern interpolation use this class.
     */
    var PluralSprintf = (function (_super) {
        __extends(PluralSprintf, _super);
        function PluralSprintf() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        /**
         * Selects the right plural pattern from the passed multi pattern string and applies that for sprintf function.
         * @param {string} patterns - A multi pattern string.
         * @param {number} count - Count value used to detemine the plural form.
         * @return {string} The result string where parameter(s) were injected to the placeholder(s).
         */
        PluralSprintf.FormatPlural = function (patterns, count) {
            return this.FormatPluralEx(patterns, count, count);
        };
        /**
         * Selects the right plural pattern from the passed multi pattern string and applies that for sprintf function.
         * @param {string} patterns - A multi pattern string.
         * @param {number} count - Count value used to detemine the plural form. count parameter is not automatically passed to sprintf but you have to include it to args.
         * @param {any} args - Optional additional arguments for sprtinf.
         * @return {string} The result string where parameter(s) were injected to the placeholder(s).
         */
        PluralSprintf.FormatPluralEx = function (patterns, count) {
            var args = [];
            for (var _i = 2; _i < arguments.length; _i++) {
                args[_i - 2] = arguments[_i];
            }
            var pattern = this.GetPluralPattern(patterns, count);
            return sprintf_js_1.sprintf(pattern, args);
        };
        /**
         * Selects the right gender pattern from the passed multi pattern string and applies that for sprintf function.
         * @param {string} patterns - A multi pattern string.
         * @param {Gender} gender - Gender value.
         * @param {any} args - Optional arguments for sprtinf.
         * @return {string} The result string where parameter(s) were injected to the placeholder(s).
         */
        PluralSprintf.FormatGender = function (patterns, gender) {
            var args = [];
            for (var _i = 2; _i < arguments.length; _i++) {
                args[_i - 2] = arguments[_i];
            }
            var pattern = this.GetGenderPattern(patterns, gender);
            return sprintf_js_1.sprintf(pattern, args);
        };
        /**
         * Selects the right gender pattern from the passed multi pattern string and applies that for sprintf function.
         * @param {string} patterns - A multi pattern string.
         * @param {any} args - Arguments for sprtinf. For plural parameter pass the number. For gender parameter pass an object containing gender and value properties.
         * @return {string} The result string where parameter(s) were injected to the placeholder(s).
         */
        PluralSprintf.Format = function (patterns) {
            var args = [];
            for (var _i = 1; _i < arguments.length; _i++) {
                args[_i - 1] = arguments[_i];
            }
            this.InitializeIndexProc();
            var result = "";
            var str = new plural_1.FormatString(patterns);
            var count = args.length;
            if (count != str.Count)
                throw new Error(sprintf_js_1.sprintf("Pattern \"%s\" does not have enough patterns for %d parameter", patterns, count));
            // Process parameter from last to first and build the result string part by part
            for (var i = str.Count - 1; i >= 0; i--) {
                var parameter = str.GetItem(i);
                var arg = args[i];
                if (typeof arg == "number") {
                    // Plural
                    var count = arg;
                    var form = plural_1.Plural.indexProc(count);
                    var pattern = parameter.GetCheckedPluralString(form, count);
                    if (i == str.Count - 1)
                        result = sprintf_js_1.sprintf(pattern, count);
                    else
                        result = sprintf_js_1.sprintf(pattern, count, result);
                }
                else {
                    // Gender
                    var keys = Object.keys(arg);
                    var value = undefined;
                    var gender = arg["gender"];
                    if (gender == undefined)
                        throw new Error("No gender parameter passed for: " + patterns);
                    for (var _a = 0, keys_1 = keys; _a < keys_1.length; _a++) {
                        var key = keys_1[_a];
                        if (key != "gender") {
                            value = arg[key];
                            break;
                        }
                    }
                    if (value == undefined)
                        throw new Error("Gender parameter does not have value parameter");
                    var pattern = parameter.GetGenderString(gender);
                    if (pattern == "")
                        pattern = parameter.DefaultValue;
                    if (i == str.Count - 1)
                        result = sprintf_js_1.sprintf(pattern, value);
                    else
                        result = sprintf_js_1.sprintf(pattern, value, result);
                }
            }
            if (str.StartPattern != "")
                result = sprintf_js_1.sprintf(str.StartPattern, result);
            return result;
        };
        return PluralSprintf;
    }(plural_1.Plural));
    exports.PluralSprintf = PluralSprintf;
});
//# sourceMappingURL=pluralsprintf.js.map