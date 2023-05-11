# Explanation of the test scripts

This document summarizes how the test scripts work, so one can add and modify test cases.


## Motivation

The purpose of these tests is to check that the code compiles without errors

* for all models (where models using different features, e.g. single-agent or multi-agent),
* with all their parametrisations
* for all binaries (filter, analyzer)
* for different compiler flags (optimizations on/off)
* for all combinations of features (log-probabilities, …)
* for all available compiler/library versions.

This is necessary, since not all code paths are actually compiled.
This is due to

* `#if` and `#ifdef` preprocessor directives (where the plan should be to eliminate most of them for the future)
  * some of them set by the model (e.g. if the model is multi-agent)
  * some of them are set per compilation to enable feature (e.g. log-probabilities)
* Template specialisations

In particular when developing, not every binary is tested by the developer.
Not speaking of testing different models - often code is changed "on demand" by a particular model.


# Architecture overview

Every example (excluding show cases of language features that do not compile as-is) will be tested.
Tests cannot completely be generalised for all models (esp. dependencies between different versions of PDDL models, problem files, observation models, observation data).
Therefore, every example has its own separate test script `test.sh` in the example's directory.
The test script `test.sh` in the example directory merely calls the `<name>/test.sh` for every example.

Functions that *can* be generalised for multiple tests are placed in the `test-functions.sh`.
Global (i.e. applying to all examples) test options are defined in `test_global.conf`.
Configuration for the tests of a specific model are defined in `<name>/test.conf`.
The file `test_local.conf` sets options for your machine (e.g. paths, compiler versions).

## Test configurations

Every example is tested with different configurations.
Some configurations are valid for all examples, e.g. optimisation flags or model-indepent features.
Other configurations depend on the model (e.g. domain files, observation models).

Every distinct flag/option is represented by an array variable with all possible values.
For instance, `tf_debug=("" "-G")` tests with optimizations enabled (`""`) and with optimisations disable (`-G`). This is an `rc` flag.

For the tests, all possible combinations of all values are tested.
For this purpose, a array variable `VARS` contains all *variable names* with different options.
For instance, `VARS` contains the *string* `tf_debug`, referring the variable `tf_debug`.

`VARS` is not defined explicitly, but by `VARS_GLOBAL` (for global options defined in `test_global.conf`) and by every example's test script.

## Implementation

The `test-functions.sh` file contains basic functions used by every test script.

Every test has to define its own set of variables/options/flags that need to be iterated over (in their `test.conf`).
These are, most importantly, `DOMAINS`, `PROBLEMS` and `OBS_MODELS`.

Usually, the test scripts have to do the following tasks:

0. source `test-functions.sh` and `test.conf`
1. Call `prepare` to set-up directories and variables
2. Call `recurse` with the `count_tests` functions.
   This counts the number of test cases for this example, so a progress (e.g. "[1/12]") can be shown.
3. Define a function `do_test` which itself calls `run_rc` with additional arguments
4. Call `recurse` with the defined `do_test` function.
5. Call `print_stats` to print a summary of successful/failed test cases.

Iterating over all combinations of flag values is done by the `recurse` function.
It sets variables prefixed with `v_` to the current value of the flags.
Then, it calls the named function (given as argument); this function has access to the values of the flags.

For instance, `tf_debug=("" "-G")`, i.e. there are two possible values for the debug flag.
When calling `recurse` with a function `do_test` as parameter, `do_test` sees a variable `v_tf_debug` with value either `""` or `"-G"`.

Usually, `do_test` only calls `run_rc` with some additional arguments (e.g. set number of particles).
The `run_rc` function calls the `rc` script with all flags set according to the variable.
In particular, it sets the domain/problem files according to `v_DOMAINS` and `v_PROBLEMS`.


# More details on the variables

In some cases, not all combinations of all values are *valid* combinations.
For instance, the Meeting example has several domain files, and each domain file requires a separate problem file and observation model.
Therefore, the Meeting example defines several sets of test cases.


## `Recurse` takes two arguments

For this purpose, the `recurse` function takes two arguments:
in addition to the function to execute, it also takes an array variable `$V` containing all *variable names* of the arrays containing all flags/options to test for the current set of test cases.
This array variable `$V` is similar to `$VARS_GLOBAL`, except that it applies to the current example/set of test cases.
In fact, all variables from `$VARS_GLOBAL` and `$V` will be merged to the variable `$VARS`.

For example, the Meeting model defines

    DOMAINS_1=("mtg1d.pddl")
    PROBLEMS_1=("mtg1p.pddl")
    VARS_1=(DOMAINS_1 PROBLEMS_1 …)

and then calls

    recurse VARS_1 do_test

for the first domain file.
If `VARS_GLOBAL=(tf_debug)` (as shown above), then the final `VARS` array contains

    VARS=(DOMAINS_1 PROBLEMS_1 tf_debug)

and thus iterates over all possible combinations of `$DOMAINS_1`, `$PROBLEMS_1` and `$tf_debug`.
*Note* that `$VARS` has not to be defined, it is automatically created when calling `recurse`.


## More sets of test cases

Furthermore, the meeting domain also defines

    DOMAINS_2=("mtg2d.pddl")
    PROBLEMS_2=("mtg2p.pddl")
    VARS_2=(DOMAINS_2 PROBLEMS_2 …)

to test the `mtg2d.pddl` domain file.

Obviously, defining different `$DOMAINS` variables with the same name is not possible.
However, the `run_rc` function expects the variable to be named `$DOMAINS` (in fact, it expects the domain file in the variable `$v_DOMAINS`, which is set by `recurse` from the `$DOMAINS` variable).

To overcome this limitation, but still be able to define different sets of test cases, the different `VARS` arrays are *associative* arrays.
Therefore, the definition of `VARS_1` actually is

    declare -A VARS_1=([DOMAINS]=DOMAINS_1 [PROBLEMS]=PROBLEMS_1 …)

This way, the `recurse` function is told to generate a variable *as-if* the array was named like the key.
In this example, a variable `$v_DOMAINS` with values from `$DOMAINS_1` will be created (instead of naming it `$v_DOMAINS_1`).


## Compiler options

Some variables are expected by the `run_rc` functions, for instance `$DOMAINS` as shown above.
However, most additional configuration options are additional flags passed to `rc` or preprocessor defines.

For instance, the `m_d.pddl` Meeting domain (tested in the third set of test cases) needs a preprocessor define to select the duration model.
It is defined as

    tf_durmodel_3=("-DDUR_EXP=1") # only a single duration model to be tested

In order for the `run_rc` function to know which variable shall be passed as additional options, it requires an additional variable `v_compiler_vars`.
Additionally, the `test_global.conf` also defines `global_compiler_vars`, which is treated equally.

For instance for the third set of test cases for the Meeting model, the per-example `compiler_vars` is defined as

    compiler_vars_3=(tf_durmodel_3) # other variables are also possible

Because different sets of test cases may have different sets of compiler variables (just like they have different domains), we also need different `compiler_vars` variables.
Which one to use has to be defined in the corresponding `VARS` array under the key `compiler_vars`.

As an example, it is defined as follows for the Meeting model:

    declare -A VARS_3=([DOMAINS]=DOMAINS_3 [PROBLEMS]=PROBLEMS_3 [compiler_vars]=\'compiler_vars_3 [tf_durmodel_3]=tf_durmodel_3)

Note that the value in the `VARS` array is the name of the variable which contains all values to iterate over.
For the case of `$v_compiler_vars`, there is only a single value that `$v_compiler_vars` takes—namely `compiler_vars_3`.
The different values of the variables *inside* `compiler_vars` must be added to the `VARS` array as well; thus, `tf_durmodel_3` is part of `$VARS_3`.

Because `compiler_vars_3` is the single value that `compiler_vars` shall take, we can set that value by using the prefix `'` in front of the value.
Otherwise, we would have to use another indirection like

    compiler_vars_3=(tf_durmodel_3) # other variables are also possible
    compiler_vars_3_values=(compiler_vars_3)
    declare -A VARS_3=(… [compiler_vars]=compiler_vars_3_values …)
