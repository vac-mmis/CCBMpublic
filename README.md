This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-nc-sa/4.0/).

![by-nc-sa](https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png "Logo CC-by-nc-sa")

For details view the [Licence file](LICENSE)!

---

# TLTR

Fast start: 
```
  docker run -it sebastianbader/ccbm
```

Go into the shoes-example:
```
  cd Examples/Shoes
```

And run the example by invoking:
```
  ./run.sh MyOutput
```

---

# Computational Causal Behaviour Models (CCBM)
The purpose of the CCBM system is to describe the behavior of dynamic systems, that is systems whose state changes over time. To achieve this, CCBM follows a symbolic approach that uses precondition-effect rules to define the causal model of the domain it operates on. The user may specifiy this domain with its possible actions, preconditions and effects as well as the problem, via the planning domain definition language (PDDL). Also specified by the user is a sequence of observations which is read by the system to estimate possible action trajectories via Bayesian filtering. The CCBM toolkit allows to generate several types of filters - Hidden Markov Model, Particle Filter or Marginal Filter - out of defined causal model. These filters in turn allow to track actions and states for the provided observations.

In order to understand the mathematical background we highly recommend everyone interested in CCBM to read [the following paper](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0109381), especially the Appendix, as well as the technical report contained in `Man/rc-techrep.pdf`.

# INSTALLING - Linux

Please follow these steps closely and read the entire file carefully for the best user experience. This README provides installation instructions for the CCBM system on Linux or MacOS plus a short introduction on how to use the system. If you are interested in implementing your own domain and want some further insights into the components of CCBM please also read the [CCBM Quickstart Guide](Man/CCBM_Quickstart_Guide.md).

## Prerequisites
The following instructions are written for Linux.
The CCBM toolkit requires:

* GCC 4.9 or higher, and the packages libtinfo-dev, libgmp3-dev, zlib1g-dev. They can be installed via the following shell commands:
    ```shell
    sudo apt install build-essential
    sudo apt install libtinfo-dev
    sudo apt install libgmp3-dev
    sudo apt install zlib1g-dev
    ```

* The specific version of GHC 9.2.2 and cabal 3.6.2.0 is confirmed to work.

    We recommend you to install the [Haskell platform][1], as it contains all necessary tools. To do so first install GHCup via curl:

    ```shell
    sudo apt install curl
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    ```
    GHCup allows to easily change the intsalled/used versions of GHC, cabal and stack.
    It also adds the ghcup-environment to your path so that you dont need to do it manually. For the changes to take effect simply restart your terminal
    before you continue with the next step or type

    ```shell
    source ~/.bashrc
    ```
    In case you want to select a specific version of GHC and cabal, after the installation of ghcup run

    ```shell
    ghcup tui
    ```
    to open the text user interface of ghcup. From there press 'a' to show all available versions. Select desired GHC and cabal versions and press 'i' to install them. Afterwards
    press 's' while they are selected to set these versions as active. Quit GHCup by pressing 'q'. You can check whether you use the right versions by running the following commands:

    ```shell
    ghc --version
    cabal --version
    ```

    After you are done, execute cabal user config update
    ```shell
    cabal user-config update
    ```

    Otherwise, one may manually install `ghc`, `cabal` and then run `cabal update && cabal install happy`.
    Refer to you distribution for installing these packages.
    The [download page of haskell][2] also has instructions for Mac OS X.

* the bash shell, version 4.1 or higher

* a Unix-like operating system (Linux or Mac OS X)


## Clone and prepare

Clone the CCBM git repository and make the following preparations:

* Check if the fann folder in CCBM/src/lib/fann is empty, if so do the following:
  git clone FANN manually from https://github.com/libfann/fann/ and put the contents of the cloned fann folder into CCBM/src/lib/fann (Make sure that you don't simply put the entire fann folder into the existing fann folder but only its contents, such that the path to the files is CCBM/src/lib/fann and not CCBM/src/lib/fann/fann.)

* Run the script getboost.sh. This script automatically downloads boost 1.64.0 and moves it to the correct place in the directory. Do this by executing the command `./getboost.sh` in the shell. If the script gives an error, try using `sed -i -e 's/\r$//' ./getboost.sh` on it to remove the carriage return and run again.

* If the previous step does not work due to a broken link or another error do the following: Download boost 1.64.0 manually from its [Sourceforge download page](https://sourceforge.net/projects/boost/files/boost/), move it to the CCBM main directory and run the following commands:
    ```shell
    tar xjf boost_1_64_0.tar.bz2
    rm boost_1_64_0.tar.bz2
    mv boost_1_64_0/ src/lib/boost/
    ```

* To run the rc script from anywhere you should also add the CCBM directory where `rc` is located to your `PATH`. If you are unsure how to do it follow the steps provided in [https://linuxize.com/post/how-to-add-directory-to-path-in-linux/]. Make sure to add the CCBM directory to your path permanently.

  [Excerpt from the link above]

  To make the change permanent, you need to define the $PATH variable in the shell configuration files. In most Linux distributions when you start a new session, environment variables are read from the following files:

    * Global shell specific configuration files such as `/etc/environment` and `/etc/profile`. Use this file if you want the new directory to be added to all system users `$PATH`.

    * Per-user shell specific configuration files. For example, if you are using Bash, you can set the `$PATH` variable in the `~/.bashrc` file. If you are using Zsh the file name is `~/.zshrc`.

  For example, to set the variable in the `~/.bashrc` file, first open the file with your text editor (ie. nano)

    `nano ~/.bashrc`

  and add the line containing the path to your CCBM directory at the end of it:

    `export PATH="$PATH:/home/username/CCBM"`

  Save the file and load the new `$PATH` into the current shell session using the source command:

    `source ~/.bashrc`

* In the main repository directory, run the command
    ```shell
    make
    ```

* If the executable file 'rcg' does not exist in CCBM/RCGen create a symbolic link to it. For me it is located in subfolders of dist-newstyle so I ran the command
    ```shell
    ln -s /home/username/CCBM/RCGen/dist-newstyle/build/x86_64-linux/ghc-9.2.2/RCGen-0.1.0.0/x/rcg/build/rcg/rcg rcg
    ```
   in the directory CCBM/RCGen which creates a symbolic link in this directory named `rcg` to the actual location of the rcg executable. Please note that the above path, particularly the parts `/ghc-9.2.2/` and `/x86_64-linux/`, needs to be changed depending on your system and the GHC version you have set up on your system. You need to find the location of the rcg file on your machine by navigating to /RCGen/dist-newstyle/build/ and from there go deeper down into subdirectories, until the location of rcg is found.

# INSTALLING - Mac OS

## Prerequisites
The following instructions are written for Mac OS and are almost the same as the ones for Linux, except for libraries and how to add CCBM directory to PATH.
The CCBM toolkit requires:

* GCC 4.9 or higher, and the packages llvm and libomp, they can be installed via the following shell commands:
    ```shell
    brew install llvm
    brew install libomp
    ```

* The specific version of GHC 9.2.2 and cabal 3.6.2.0 is confirmed to work.

    We recommend you to install the [Haskell platform][1], as it contains all necessary tools. To do so first install GHCup via curl:

    ```shell
    sudo apt install curl
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    ```
    GHCup allows to easily change the intsalled/used versions of GHC, cabal and stack.
    It also adds the ghcup-environment to your path so that you dont need to do it manually. For the changes to take effect simply restart your terminal
    before you continue with the next step or type

    ```shell
    source ~/.bashrc
    ```
    In case you want to select a specific version of GHC and cabal, after the installation of ghcup run

    ```shell
    ghcup tui
    ```
    to open the text user interface of ghcup. From there press 'a' to
    show all available versions. Select desired GHC and cabal versions and press 'i' to install them. Afterwards press 's' while they are selected to set these versions as active. Quit GHCup by pressing 'q'. You can check whether you use the right versions by running the following commands:

    ```shell
    ghc --version
    cabal --version
    ```

    After you are done, execute cabal user config update
    ```shell
    cabal user-config update
    ```


    Otherwise, one may manually install `ghc`, `cabal` and then run `cabal update && cabal install happy`.
    Refer to you distribution for installing these packages.
    The [download page of haskell][2] also has instructions for Mac OS X.

* the bash shell, version 4.1 or higher

* a Unix-like operating system (Linux or Mac OS X)


## Clone and prepare

Clone the CCBM git repository and make the following preparations:

* Check if the fann folder in CCBM/src/lib/fann is empty, if so do the following:
  git clone FANN manually from https://github.com/libfann/fann/ and put the contents of the cloned fann folder into CCBM/src/lib/fann (Make sure that you don't simply put the entire fann folder into the existing fann folder but only its contents, such that the path to the files is CCBM/src/lib/fann and not CCBM/src/lib/fann/fann.)

* Run the script getboost.sh. This script automatically downloads boost 1.64.0 and moves it to the correct place in the directory. Do this by executing the command `./getboost.sh` in the shell. If the script gives an error, try using `sed -i -e 's/\r$//' ./getboost.sh` on it to remove the carriage return and run again.

* If the previous step does not work due to a broken link or another error do the following: Download boost 1.64.0 manually from its [Sourceforge download page](https://sourceforge.net/projects/boost/files/boost/), move it to the CCBM main directory and run the following commands:
    ```shell
    tar xjf boost_1_64_0.tar.bz2
    rm boost_1_64_0.tar.bz2
    mv boost_1_64_0/ src/lib/boost/
    ```

* To run the rc script from anywhere you should also add the CCBM directory where rc is located to your path. To do so, type:
    ```shell
    sudo vi /etc/paths
    ```

    and add the path to the CCBM directory.

* In the main repository directory, run the command
    ```shell
    make
    ```

* If the executable file `rcg` does not exist in CCBM/RCGen create a symbolic link to it. For me it is located in subfolders of dist-newstyle so I ran the command
    ```shell
    ln -s /home/username/CCBM/RCGen/dist-newstyle/build/x86_64-linux/ghc-9.2.2/RCGen-0.1.0.0/x/rcg/build/rcg/rcg rcg
    ```
   in the directory CCBM/RCGen which creates a symbolic link in this directory named `rcg` to the actual location of the rcg executable. Please note that the above path, particularly the parts `/ghc-9.2.2/` and `/x86_64-linux/`, needs to be changed depending on your system and the GHC version you have set up on your system. You need to find the location of the rcg file on your machine by navigating to /RCGen/dist-newstyle/build/ and from there go deeper down into subdirectories, until the location of rcg is found.

# USING

The main script to compile models is `rc` in the main directory.
In its basic form, it suffices to run

```shell
./rc -d $domain.pddl -p $problem.pddl -o $observation_model
```

This generates a couple of binaries.
Using the `-w` option to specify a location for intermediate files (such as object files) is recommended.
All available parameters can be displayed using `--help` option.

If the `./rc` script throws an error upon running (might happen depending on your system) one can fix this by running the command
```shell
sed -i -e 's/\r$//' ./rc
```
this replaces the windows carriage return char by an empty string.

You can run a filter by executing
```shell
./$model-filter < $observations > $filtered_output
```

*Have a look at the Examples in the `Examples/` directory and try out one of the scripts.*
To run an example script do the following:

Within a chosen example directory (e.g. CCBM/examples/Shoes), run
```shell
./run_test.sh out
```
In the created directory /out you find the executables filter, validator, analyzer, etc. ...

To check whether your local installation works correctly, reference outputs have been generated for each of the examples. They have been stored in the directory `old_out`. A comparison of these reference outputs with the actual outputs from your installation can be done automatically by running a script `Examples/run_similarity_test.sh`, which runs every single example and does the output comparison.

```shell
./run_similarity_test.sh
```

In order to develop an observation model on your own, check out the tutorial `Man/CCBM_Quickstart_Guide.md`.

# Current development - Viterbi filter
Currently we are working on adding the Viterbi filter to CCBM. An experimental version is already included, but is not yet stable and tested, use at your own risk.

# Licenses

This product includes and makes use of third-party software:

* Valgrind (http://valgrind.org/) headers only
  * BSD-like license
  * no restrictions on derivative work as long as source is unchanged

* Boost (http://www.boost.org/)
  * Boost license
  * copyright notice for source distributions of derivative work (included in lib/boost)

* libfann (https://github.com/libfann/fann/)
  * LGPL 2.1
  * can be used in derivative work (as long as it can be linked separately)
  * fann uses no C++ templates, thus can be linked separately

[1]: https://www.haskell.org/downloads#platform
[2]: https://www.haskell.org/downloads

# Authors
CCBM has been developed by the Mobile Multimedia Information Systems (MMIS) group at the University of Rostock (Germany).

Group website: https://www.mmis.informatik.uni-rostock.de/

Email address for inquiries regarding CCBM: ccbm@uni-rostock.de

# Citing CCBM
Please cite [the following paper](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0109381) when using CCBM:

Krüger, F., Nyolt, M., Yordanova, K., Hein, A., & Kirste, T. (2014). Computational state space models for activity and intention recognition. A feasibility study. *PloS one, 9(11)*, e109381.

```
@article{kruger2014computational,
  title={Computational state space models for activity and intention recognition. A feasibility study},
  author={Kr{\"u}ger, Frank and Nyolt, Martin and Yordanova, Kristina and Hein, Albert and Kirste, Thomas},
  journal={PloS one},
  volume={9},
  number={11},
  pages={e109381},
  year={2014},
  publisher={Public Library of Science San Francisco, USA}
}
```
