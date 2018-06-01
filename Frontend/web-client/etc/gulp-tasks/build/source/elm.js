"use strict";
const paths = require('../../paths.js');
let gulp = require('gulp');
let elm  = require('gulp-elm');
let plugins = require('gulp-load-plugins')();

gulp.task('elm-init', elm.init);

gulp.task('build:source:elm', ['elm-init'], function(){
  return gulp.src(paths.source.elm)
    .pipe(plugins.plumber({errorHandler: plugins.notify.onError('Error: <%= error.message %>')}))
    .pipe(elm.bundle("app.js", {filetype: 'js', debug: false }))
    .pipe(gulp.dest(paths.target));
});