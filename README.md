r7.rs-compiler
==============

[![Build Status](https://travis-ci.org/naominitel/r7.rs-compiler.png?branch=master)](https://travis-ci.org/naominitel/r7.rs-compiler)

This is a project of a full [R7RS](http://www.scheme-reports.org/2013/working-group-1.html) Scheme implementation.
It aims at being as interoperable as possible with other Scheme implementations, which means:

  * Fully compliant to the R7RS standard
  * Implement as many SRFIs as possible

Another goal of the project is to make a good scripting language that can be used for systems scripting,
application plugins, etc.

The project consists of two main parts:

  * A bytecode compiler written in Haskell
  * A fast, [lightweight virtual machine written in Rust](https://github.com/naominitel/r7.rs-vm/)

### Progression

Currently, the compiler is able to build very simple Scheme programs that consists of a single program file.
Libraries and imports as defined by the section 5 of R7RS are not yet supported.

### Building and dependencies

You just need a recent version of GHC and cabal to build.
The compiler can be simply compiled with:

```shell
# Using a cabal sandbox is recommanded.
# cabal sandbox init
cabal configure

# Install requested dependencies with cabal install.
cabal build
```

### Usage

```
Usage:

    scmc [option(s)|filename]

Common options are:
    -s: Assemble only. Don't generate bytecode
    -o file: Write output to file instead of out.bin
    -h: Display this message

Example:

> scmc file.scm
```

### Running a compiled script

For running a compiled script you must use the associed virtual machine written in Rust.
Disponible here: https://github.com/naominitel/r7.rs-vm/
