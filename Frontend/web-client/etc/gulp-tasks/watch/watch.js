"use strict";
let gulp = require('gulp');

gulp.task('watch', [
    'watch:source:elm',
    'watch:source:css',
    'watch:css-framework',
]);