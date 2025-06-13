# gambenv - Gambit Scheme quick setup and version manager

This repository contains script to organize, manage and isolate Gambit Scheme
compiler versions. We take inspiration from Python's `venv` module and
`virtualenvwrapper` tool.

This code is currently only developed and tested on Linux (x86_64 Debian
trixie). Expect breaking changes, notably in the naming of things.

What follows is an overall description of the goals and mechanics of the
scripts. Please read the scripts themselves for more information.

## `gambenv.sh`

The `gambenv.sh` script contains an installer for the Gambit Scheme compiler. It
sets up the `~/.gambit_env` directory and subdirectories and clones a master
copy of the Gambit Scheme sources.

- The `sources` subdirectory contains copies of the Gambit Scheme sources. It is
where the compilation will happen.
- The `versions` subdirectory contains one subdirectory per Gambit Scheme
compiler version. A *version* is akin to a target triple, except with
potentially many `configure` flags in use (`CC`, `--enable-single-host`,
`--enable-multiple-threaded-vms`, etc). Every version is identified by a
*version alias*. Version aliases are strings such as `496-sh` which could mean
Gambit v4.9.6 compiled with the default `CC` and `--enable-single-host`. Version
alias naming is entirely up to the user. Every version subdirectory contains its
own `~~lib` and `~~userlib`. **NOTE: We will be unifying and
`version` and `env` soon.**
- The `env` subdirectory contains shell scripts that can be sourced to modify
the current Gambit Scheme compiler. These can be sourced like
`venv/bin/activate` scripts.
- The `current` subdirectory contains the current *global* Gambit Scheme
compiler version. It contains the version installed by the `gambenv.sh` script
and should be left alone.
- The `bin` subdirectory contains the binaries to all of the compiled version
binaries. Every binary (except the default) is suffixed with its version alias
such as `gsi@496-sh`. This directory must be added to your shell's `PATH` in
order to use the binaries. It then becomes trivial to launch any gambit
version using its proper version alias.

## `gambit.sh`

The `gambit.sh` script contains the various shell functions used to manage
Gambit Scheme environments. This script must be sourced in your shell's `rc`
file so you can use the `gambit` function. Read the script for more details.

## Others

One goals of this tool is to easily build and use a matrix of Gambit Scheme
versions in Docker. The `Makefile` contains recipes to do that. The
`simple-builder.sh` file has some utilities to facilitate this process. The
build matrix is currently hardcoded. It will eventually be much easier to define
new `configure` flags, `CC` versions, etc.
