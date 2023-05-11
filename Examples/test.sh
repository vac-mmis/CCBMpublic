#!/usr/bin/env bash

source test_global.conf
source test-functions.sh

prepare # maybe create common temp directory

# execute test scripts for every example
while read testscript; do
    directory=$(dirname "$testscript")
    pushd "$directory" > /dev/null

    echo -e "Running tests for $COL_BOLD$(basename $directory)$COL_NORM"
    source test.sh

    popd > /dev/null
done < <(find "$dir" -mindepth 2 -name test.sh | sort)

print_collected_stats
