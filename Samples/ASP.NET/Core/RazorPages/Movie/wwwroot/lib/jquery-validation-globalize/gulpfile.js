var gulp = require("gulp"),
    util = require("gulp-util"),
    qunit = require("gulp-qunit");

gulp.task("default", function() {
    
    var tests = "./test/tests.html"; 
    
    util.log("Running these tests: " + tests);
    
    return gulp
        .src(tests)
        .pipe(qunit());
});