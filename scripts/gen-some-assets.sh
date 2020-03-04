#!/bin/bash

set -e
set -u

function check_file_exists() {
  [ -e "$1" ] || { echo "$1 should exist"; exit 1; }
}

function check_dir_exists() {
  [ -d "$1" ] || { echo "$1 should exist"; exit 1; }
}

function run() {
  echo "$@"
  "$@"
}

declare -r FILE_1616="assets/16x16.png"
declare -r FILE_2424="assets/24x24.png"
declare -r DEST_DIR="cardgen/src/commonMain/resources"

check_file_exists "$FILE_1616"
check_file_exists "$FILE_2424"
check_dir_exists "$DEST_DIR"

declare -r FILE_1616_FILENAME=$(basename $FILE_1616)
declare -r FILE_1616_x2="$DEST_DIR/"${FILE_1616_FILENAME%".png"}"_x2.png"
run convert $FILE_1616 -scale 200% $FILE_1616_x2

declare -r FILE_2424_FILENAME=$(basename $FILE_2424)
declare -r FILE_2424_x3="$DEST_DIR/"${FILE_2424_FILENAME%".png"}"_x3.png"
run convert $FILE_2424 -scale 300% $FILE_2424_x3

run cp "$DEST_DIR/data.json" "ai/."
