var Sample = (function () {
    function Sample(value, count) {
        this.value = value;
        this.count = count;
    }
    Sample.prototype.start = function () {
        var str = this.value;

        for (var i = 0; i < this.count; i++)
            str = this.value;
    };

    Sample.prototype.stop = function () {
        this.value = "";
    };
    return Sample;
})();
//# sourceMappingURL=app.js.map
