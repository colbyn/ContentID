"use strict";
const paths      = require('../paths.js');
let gulp         = require('gulp');
let plugins      = require('gulp-load-plugins')();
let mqpacker     = require('css-mqpacker');
let csswring     = require('csswring');
let classPrfx    = require('postcss-class-prefix');

const processors = [
    require('postcss-import'),
    require('postcss-nested'),
    require('postcss-cssnext'),
    mqpacker,
    csswring
];

gulp.task('build:css-framework',()=>{
    return  gulp.src(paths.css_framework)
        .pipe(plugins.changed(paths.target, {extension: '.framework.css'}))
        .pipe(plugins.plumber({errorHandler: plugins.notify.onError('Error: <%= error.message %>')}))
        .pipe(plugins.sourcemaps.init())
            .pipe(plugins.concat('bundle.framework.css'))
        .pipe(plugins.postcss(processors))
        .pipe(plugins.sourcemaps.write('.')) // idk about this path
        .pipe(gulp.dest(paths.target));
});