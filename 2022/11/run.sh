#!/bin/sh
#
# usage: ./run.sh <input>

set -e

gnatmake monkey_gen
./monkey_gen < "$1" > monkey_defs.adb
gnatmake -g advent_11_1
./advent_11_1
gnatmake -g advent_11_2
./advent_11_2
