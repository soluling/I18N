define(["require", "exports", "./language", "./pluralsprintf"], function (require, exports, language_1, pluralsprintf_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    function setValue(element, count) {
        document.getElementById(element).innerHTML = pluralsprintf_1.PluralSprintf.FormatPlural("one;%d file;other;%d files", count); //loc: Plural enabled message pattern. There has to be as many parts (separated by ;) as there are plural forms in the language.
        document.getElementById(element + "z").innerHTML = pluralsprintf_1.PluralSprintf.FormatPlural("zero;No files;one;%d file;other;%d files", count); //loc
    }
    function setMultiValue(element, completed, total) {
        document.getElementById(element).innerHTML = pluralsprintf_1.PluralSprintf.Format("I have completed %s;one;one %2$s;other;%d %s;next;one;out of one step;other;out of %d steps", completed, total); //loc
        document.getElementById(element + "z").innerHTML = pluralsprintf_1.PluralSprintf.Format("I have completed %s;zero;none %2$s;one;one %2$s;other;%d %s;next;zero;out of none steps;one;out of one step;other;out of %d steps", completed, total); //loc
    }
    language_1.Language.SetLanguage("en"); //loc: Current language
    setValue("p0", 0);
    setValue("p1", 1);
    setValue("p2", 2);
    setValue("p3", 3);
    setValue("p4", 4);
    setValue("p5", 5);
    setValue("p11", 11);
    setValue("p21", 21);
    setValue("p101", 101);
    setValue("p111", 111);
    setMultiValue("p0_0", 0, 0);
    setMultiValue("p0_1", 0, 1);
    setMultiValue("p1_1", 1, 1);
    setMultiValue("p0_2", 0, 2);
    setMultiValue("p1_2", 1, 2);
    setMultiValue("p2_2", 2, 2);
    setMultiValue("p1_3", 1, 3);
    setMultiValue("p2_3", 2, 3);
    setMultiValue("p1_10", 1, 10);
    setMultiValue("p5_10", 5, 10);
});
//# sourceMappingURL=main.js.map