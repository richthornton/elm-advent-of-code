#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
DAY_NO_ZEROS="$(echo $DAY | sed 's/^0*//')"
INPUT="Template.elm"
OUTPUT="Year${YEAR}/Day${DAY}.elm"
PUZZLE_URL="https://adventofcode.com/${YEAR}/day/${DAY_NO_ZEROS}/input"
PUZZLE_FILE="input${YEAR}${DAY}.txt"
AOC_SESSION_COOKIE="53616c7465645f5f0ef2d8e9905937cf2209f15f8a467053ab38daf8453c14f98b07db4020c6438e813694005bc9693e"

curl "${PUZZLE_URL}" -H "cookie: session=${AOC_SESSION_COOKIE}" -o "${PUZZLE_FILE}" 2>/dev/null
mkdir -p "$(dirname ${OUTPUT})"
cp "${INPUT}" "${OUTPUT}"
sed -i "" "s/YearXXX/Year${YEAR}/g" "${OUTPUT}"
sed -i "" "s/DayXXX/Day${DAY}/g" "${OUTPUT}"
sed -i "" -e "/InputXXX/r ${PUZZLE_FILE}" -e "/InputXXX/d" "${OUTPUT}"

# tmux split-window -h \; \
#   send-keys -t 0 "vim ${OUTPUT}" C-m \; \
#   send-keys -t 1 "./watch.sh ${YEAR} ${DAY}" C-m
