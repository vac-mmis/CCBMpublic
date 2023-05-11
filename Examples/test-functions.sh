if [ -t 1 ]; then # if output is a terminal, use colours for better readability
    COL_RED="\e[31m"
    COL_GREEN="\e[32m"
    COL_YELLOW="\e[1;33m"
    COL_BOLD="\e[1m"
    COL_NORM="\e[0m"
fi

dir="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"

# Manages that the variable $1 gets the proper value $2.
# In particular, after setting the value and recursing, it unsets the variable
# Unsetting is important, as some test cases might not define a value,
# the variable must be unset - otherwise it would get a value that is not designed for this test case
# $1: name
# $2: value
# $3…: remaining parameters for recurse
function recurse_set() {
    local v_name="$1"
    local v_value="$2"
    shift 2
    eval "local ${v_name}"'="${v_value}"'
    recurse "$@"
    eval "unset ${v_name}"
}

# Iterates over all values of all variables in array VARS and executes "func"
# for every combination of values.
# Call it with "recurse VARS_x func"
#
# VARS_x must be an associative array of variable names, each entry [N]=X
# denotes another array variable $N with values from $X.
# The values from $X are available in variables $v_N.
#
# For example, when VARS_x=([foo]=foo [bar]=baz),
# with foo=(1 2) and baz=(a b), then "func" is called four times, with
# the following variables set:
# v_foo=1 v_bar=a
# v_foo=1 v_bar=b
# v_foo=2 v_bar=a
# v_foo=2 v_bar=b
#
# A special exception is made when X starts with '. In this case,
# X is not interpreted as a variable name, but as the value itself.
# Thus $v_N=*, where "*" is the part after '.
# This is used if you do not want iterate over different values, but set
# a fixed value.
#
# Parameters:
# $1: array which should be applied to VARS
# $2: function to call
# $3: index of the VARS array
function recurse() {
    if [[ $# -eq 2 ]]; then
        # indexes not set, initialize with starting value
        update_vars $1
        recurse $1 $2 0
        return
    fi

    # we need the keys of the associative array VARS
    local keys=($(indir_keys VARS))

    if [[ $3 -eq ${#keys[@]} ]]; then
        # all variables set, execute function
        $2
        return $?
    fi

    # iterate over variable $1 by using indirection
    # indirection uses syntax ${!name}, where $name is a variable containing the variable name
    # in case of arrays, $name must include [@] (${!name[@]} means the indices of $name
    # and is not an indirection)

    # $name is the name of the actual variable that stores the information (e.g. DOMAINS_2)
    # ${keys[$3]} is the name of the variable that is assigned to / used by run_rc

    # test if we have a preset value via command line set
    # if yes, use that value, if no, iteratore over all possible values

    # this test uses the ${A+x} expansion: if $A is set, returns x, else nothing
    # this also works if the value is an empty string
    if [ -n "${PRESET_VALUES[${keys[$3]}]+x}" ]; then
        recurse_set "v_${keys[$3]}" "${PRESET_VALUES[${keys[$3]}]}" $1 $2 $(($3 + 1))
    else
        local name=$(indir_val VARS ${keys[$3]})
        if [[ "$name" == \'* ]]; then
            # special case: fixed value
            recurse_set "v_${keys[$3]}" "${name:1}" $1 $2 $(($3 + 1))
        else
            local name_array="${name}[@]"
            local values=("${!name_array}")

            if [ ${#values[@]} -eq 0 ]; then
                echo -e "${COL_RED}${COL_BOLD}Error:${COL_NORM} variable ${keys[$3]} ($name) is empty, no test case will be executed."
            fi

            local value
            for value in "${values[@]}"; do
                recurse_set "v_${keys[$3]}" "$value" $1 $2 $(($3 + 1))
            done
        fi
    fi
}

function usage() {
cat <<EOF
Options:
    -s                  show parameters with which the rc script is called
    -q                  don't show compiler output
    -j      <number>    sets the number of parallel jobs for the rc script
    --test  <number>    calls only special testcase, specified by <number>
    --tf_<name>=<value> fix the value of option <name> to <value>, instead of
                        of iterating over all possible values
EOF
}

# $1: compiler binary (e.g. "g++" or "/usr/bin/clang++")
function get_compiler_version() {
    local VER
    case $(basename "$1") in
        g++|clang++) VER=$("$1"  -dumpversion);;
        *) VER=$("$1" --version | head -n1);; # generic case which should be supported by most programs
    esac
    echo $VER
}

# check if local test config exists and write empty config into it
function check_if_config_exists() {
    if [ ! -f "$config_file_name" ]; then
        echo "# Global Configuration for compiler, etc.
## Edit and uncomment the lines below

tf_compiler=(\"/usr/bin/g++\" \"/usr/bin/clang++\")

# where the generated binaries and test data is saved
# relative directories are interpreted relative to the model directory, absolute paths are also possible
BUILD_DIRECTORY=test

# if set to 1, ignore \$BUILD_DIRECTORY and always create a temporary directory
USE_TEMP_BUILD_DIRECTORY=0

# location of the rc-script
RC_LOCATION=$(dirname $dir)" > $config_file_name
        return 1
    fi
    return 0
}

function check_config() {
    if [ ${#tf_compiler[@]} -eq 0 ]; then
        echo "No compiler specified in test_local.conf"
        return 1
    fi
    for c in "${tf_compiler[@]}"; do
        if [ ! -x $c ]; then
            echo "Can't execute compiler: "$c
            return 1
        fi
    done
    return 0
}

if [ ${#VARS_GLOBAL[@]} -eq 0 ]; then
    source "$dir/test_global.conf"
fi

config_file_name="$dir/test_local.conf"

if [ -z "$RC_LOCATION" ]; then
    # check config
    if ! check_if_config_exists; then
        echo "No configuration exists. Edit test_local.conf!"
        exit
    fi
    source $config_file_name
    if ! check_config; then
        exit
    fi
fi

# preset values (by command line) for certain flags/options
# this is an associative array from the variable name to the value
declare -A PRESET_VALUES

function parse_parameters() {
    while [ $# -ge 1 ]; do
        case "$1" in
            -q)     quiet=1 ;;
            -s)     show=1 ;;
            --test) test_case="$2"; shift;;
            -j)     parallel_jobs="$2"; shift;;
            -j*)    parallel_jobs="${1:2}";;
            --tf_*) local name="${1:2}"
                    name="${name%%=*}"
                    local value="${1#*=}"
                    PRESET_VALUES["$name"]="$value"
                    ;;
            *)      usage $0; exit;;
        esac
        shift
    done
}

parse_parameters "$@"

# prepare and run rc script
# for this we need:
model_name=""
model_dir=""
build_dir=""

# $1 model_dir
function prepare() {
    if [ -z $1 ]; then # global test.sh call
        export global_test=1 # set variable that we are running all tests
        if [[ $USE_TEMP_BUILD_DIRECTORY -eq 1 ]]; then
            global_build_dir="$(mktemp -d --tmpdir "ccbm-test.XXXX")" #create global build_dir
        fi
    else
        model_dir=$1
        model_name="$(basename "$model_dir")"
        if [ ! -z $global_build_dir ]; then
            build_dir="$global_build_dir/$model_name"
        else # no global build directory
            if [[ $USE_TEMP_BUILD_DIRECTORY -eq 1 ]]; then
                build_dir="$(mktemp -d --tmpdir "ccbm-test.$model_name.XXXX")"
            else
                build_dir="$model_dir/$BUILD_DIRECTORY"
            fi
        fi
        echo "build logs will be saved in $build_dir/"
    fi
}

# need a test count variable
count=1
max_count=0
passed=0
failed=0

# get the total number of test for an example
# if more variables were added to VARS in order to change the number of tests,
# that should be run through, this function also has to be changed
function count_tests() {
    max_count=$(( $max_count + 1 ))
}

# calls the rc script and passes the rest $@ to it
# $1 filter string
# $2 numberof particles
# $3 filter name
function run_rc() {
    # if only specific test cases shall be executed, skip the other
    if [[ $test_case && $test_case != $count ]]; then
        count=$(($count + 1))
        return
    fi

    working_dir="$build_dir/$count"
    mkdir -p "$working_dir"
    local rc_script="$RC_LOCATION/rc"
    local filter=$1
    local num_particles=$2
    local filter_name=$3
    shift 3

    # string summarizing the most important flags, to indicate which test-case is currently run
    local flag_summary=""

    # construct the rc call
    local rc_call=("$rc_script")
    rc_call+=("-d" "$v_DOMAINS" "-p" "$v_PROBLEMS")
    rc_call+=("-M" "$filter" "-n" "$num_particles")
    rc_call+=("-w" "$working_dir/" "-f" "$working_dir/$filter_name")

    # iterate over all values v_tf_… of the variables in global_rc_vars
    for tf in $(indir_keys global_rc_vars); do
        local varname="v_$(indir_val global_rc_vars $tf)"
        if [ -n "${!varname}" ]; then
            rc_call+=(${!varname})
            flag_summary+=" ${!varname}"
        fi
    done

    # iterate over all values v_tf_… of the variables in global_compiler_vars
    for tf in $(indir_keys global_compiler_vars); do
        local varname="v_$(indir_val global_compiler_vars $tf)"
        if [ -n "${!varname}" ]; then
            rc_call+=("-C${!varname}")
            flag_summary+=" ${!varname}"
        fi
    done

    # static compiler flags for this experiment
    if [ -n "$compiler_flags" ]; then
        rc_call+=("-C$compiler_flags")
    fi

    if [ -n "$v_compiler_vars" ]; then
        # iterate over all values v_tf_… of the variables in compiler_vars for this model
        # because compiler_vars is different for every model, it is indirectly
        # stored in v_compiler_vars
        for tf in $(indir_keys "$v_compiler_vars"); do
            local varname="v_$(indir_val "$v_compiler_vars" $tf)"
            if [ -n "${!varname}" ]; then
                rc_call+=("-C${!varname}")
                flag_summary+=" ${!varname}"
            fi
        done
    fi

    if [ -n "$parallel_jobs" ]; then
        rc_call+=("-j" "$parallel_jobs")
    fi

    rc_call+=("$@")

    declare -A exports=([CXX]="$v_tf_compiler")
    local exports_string=""

    for v in ${!exports[@]}; do
        export $v="${exports[$v]}"
        exports_string+="$v=${exports[$v]} "
    done

    # show/log the rc call
    if [ -n "$show" ]; then
        echo "${exports_string}${rc_call[*]}"
    fi
    echo "${exports_string}${rc_call[*]}" > "$working_dir/tmp.log"

    # run rc
    if [ -n "$quiet" ]; then
        echo -n "[$count/$max_count] ($v_DOMAINS $v_PROBLEMS)${flag_summary} "
        "${rc_call[@]}" &>> "$working_dir/tmp.log"
    else
        echo "[$count/$max_count] ($v_DOMAINS $v_PROBLEMS)"
        "${rc_call[@]}" 2>&1 | tee -a "$working_dir/tmp.log"
    fi

    # evaluate result
    if [ ${PIPESTATUS[0]} -eq 0 ]; then
        echo -e "=> $COL_GREEN""pass""$COL_NORM"
        mv "$working_dir/tmp.log" "$working_dir/rc.log_out"
        passed=$(( $passed + 1 ))
    else
        echo -en "=> $COL_RED""fail""$COL_NORM "
        mv "$working_dir/tmp.log" "$working_dir/rc.log_err"
        echo "(log: $working_dir/rc.log_err)"
        failed=$(( $failed + 1 ))
    fi

    # delete object files and binaries
    find "$working_dir" -type f \( -name *.o -o -perm -111 \) -delete
    find "$working_dir" -type d -empty -delete # delete empty directories

    count=$(($count + 1))
}

# returns all indices (keys) of an associative array $1
function indir_keys() {
    eval "echo \${!$1[@]}"
}

# returns the value of an associative array $1 at key $2
function indir_val() {
    eval "echo \${$1[$2]}"
}

# updates VARS array: combines the associative arrays VARS_GLOBAL and the array named in $1 into VARS
# $1 array name
function update_vars() {
    # start with a clean $VARS array, otherwise entries from older test cases will remain
    local key
    for key in "${!VARS[@]}"; do
        unset VARS[$key]
    done

    local name=$1
    for i in $(indir_keys VARS_GLOBAL); do
        VARS[$i]="$(indir_val VARS_GLOBAL $i)";
    done
    for i in $(indir_keys $name); do
        VARS[$i]="$(indir_val $name $i)";
    done

    # add variables that are configure in test_local.conf
    local local_VARS=(tf_compiler)
    for i in "${local_VARS[@]}"; do
        VARS[$i]=$i;
    done
}

# prints the stats, every example has to call this function
function print_stats() {
    if [ "$global_test" != 1 ]; then
        # if we are not within a global test, i.e. only a single model is tested, immediately print
        # the test statistics
        printf "Test results: %3d passed, %3d failed\n" "$passed" "$failed"
    else
        # otherwise, collect the statistics
        collected_stats+=("$(printf "%12s: %3d passed, %3d failed" "$model_name" "$passed" "$failed")")
        global_passed=$(( $global_passed + $passed ))
        global_failed=$(( $global_failed + $failed ))
    fi
}

function print_collected_stats() {
    printf "\nTest results: %3d passed, %3d failed\n" "$global_passed" "$global_failed"
    printf "%s\n" "${collected_stats[@]}"
}
