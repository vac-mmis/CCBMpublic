#!/usr/bin/env bash

source ../test-functions.sh
source ../test_local.conf
source ./test.conf

MODEL_DIR="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"

# create test folder and copy models into it

prepare "$MODEL_DIR"

recurse VARS_1 count_tests

# function for generating
function do_test() {
    run_rc "$FILTERS" 100 "$FILTERNAME" -O -W -r1
}

recurse VARS_1 do_test

print_stats
