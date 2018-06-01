"use strict";

const paths = {
    "source": {
        "elm": ["src/**/*.elm", "app/**/*.elm", "prelude/**/*.elm", "libs/**/*.elm"],
        "css": ["src/**/*.css", "app/**/*.css", "prelude/**/*.css", "libs/**/*.css"],
    },
    "css_framework": "etc/css-framework/**/*.css",
    "target": "etc/build/app/",
    "static-dir": "etc/build/static/",
};

module.exports = paths;