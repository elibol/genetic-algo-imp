# Genetic Algo Solution to Knapsack Problem #

### Setup ###

* [Get scmutils](http://groups.csail.mit.edu/mac/users/gjs/6946/linux-install.htm) or use the included build.
* Extract files: `tar -xf scmutils-20140302-x86-64-gnu-linux.tar`

Depending on your platform, you may have to build from source.

### Run ###

* Enter REPL: `./mit-scheme test.scm`
* Execute:
    * `(full-run)` to run **128** iterations on a pop size of **16** with mutation prob of **0.8**.
    * See source of `test.scm` for more tests.
* Exit: `(exit)`

### Notes ###

* `main.scm` is an implementation of the algorithm.
* `algorithm.scm` is a pseudo code outline of the algorithm.
