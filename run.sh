#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
INPUT="Year${YEAR}/Day${DAY}.elm"
OUTPUT="js/${YEAR}-${DAY}.js"
MODULE="Year${YEAR}.Day${DAY}"

clear;
tput reset;
elm make "${INPUT}" --output "${OUTPUT}" && \
echo "this.Elm.${MODULE}.init();" >>"${OUTPUT}" && \
echo "--------------------------------------" && \
node "${OUTPUT}";
