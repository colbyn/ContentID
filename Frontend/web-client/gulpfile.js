"use strict";
let gulp = require('gulp');

require('require-dir')('etc/gulp-tasks/build');
require('require-dir')('etc/gulp-tasks/build/source');

require('require-dir')('etc/gulp-tasks/watch');
require('require-dir')('etc/gulp-tasks/watch/source');


gulp.task('default', ["watch"]);