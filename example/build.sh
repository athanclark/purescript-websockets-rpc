#!/bin/bash

mkdir -p dist/ && \
    pulp build && \
    psc-bundle output/**/*.js -m Main --main Main > dist/Main.bundle.js && \
    browserify dist/Main.bundle.js > dist/Main.browserified.js
