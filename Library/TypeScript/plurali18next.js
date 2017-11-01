"use strict";
var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
var plural_1 = require("./plural");
var i18next = require("i18next");
/**
 * Implements function for i18next.
 */
var Plurali18next = (function (_super) {
    __extends(Plurali18next, _super);
    function Plurali18next() {
        _super.apply(this, arguments);
    }
    /**
     * Gets an i18next resource without performing interpolation.
     * @param {string} key - The i18next key of a multi-pattern strings resource.
     * @param {string} value - The original value.
     * @return {string} The plain result string.
     */
    Plurali18next.t = function (key, value) {
        var options = {
            defaultValue: value,
            interpolation: {
                prefix: "{{{{",
                suffix: "}}}}"
            }
        };
        return i18next.t(key, options);
    };
    return Plurali18next;
}(plural_1.Plural));
exports.Plurali18next = Plurali18next;
//# sourceMappingURL=plurali18next.js.map