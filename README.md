This is a project of a full [R7RS](http://www.scheme-reports.org/2013/working-group-1.html) Scheme implementation.
It aims at being as interoperable as possible with other Scheme implementations, which means: 

  * Fully compliant to the R7RS standard
  * Implement as many SRFIs as possible

Another goal of the project is to make a good scripting language that can be used for systems scripting, 
application plugins, etc.

The project consists of two main parts:

  * A bytecode compiler written in Haskell
  * A fast, lightweight virtual machine written in Rust

### Progression  

Currently, the compiler is able to build very simple Scheme programs that consists of a single program file.
Libraries and imports as defined by the section 5 of R7RS are not yet supported.

### Usage

The compiler can be simply compiled with:

```shell
ghc -XExistentialQuantification --make main.hs
```

You just need a recent version of GHC to build. 
The compiler can then be run with:

```
./main file.scm
```

The option ```-o out``` can be used to specify output file. The option ```-s``` can be used to output text assembly
instead of binary.
