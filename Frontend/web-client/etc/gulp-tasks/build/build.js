"use strict";

const gulp = require('gulp');

gulp.task('build', [
    'build:source:elm',
    'build:source:css',
    'build:css-framework'
]);