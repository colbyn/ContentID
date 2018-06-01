"use strict";
let gulp    = require('gulp');
const paths = require('../../paths.js');
const path    = require('path');
const chalk   = require('chalk');

function logChange(event) {
    if (event.type === 'added') {
        console.log("gulp log -" + chalk.green("Created: " + event.path));
    } else if (event.type === 'changed') {
        console.log("gulp log -" + chalk.yellow("Modified: " + event.path));
    } else if (event.type === 'deleted') {
        console.log("gulp log -" + chalk.red("Deleted: " + event.path));
    }
}

gulp.task('watch:source:css', ['build:source:css'],()=>{
    gulp.watch(paths.source.css,  ['build:source:css']).on('change', logChange);
});