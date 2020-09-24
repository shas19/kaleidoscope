
Haskell LLVM Tutorial
=====================

This is a JIT compiler for a tiny programming language called quick using llvm-hs 9.0. It is based on the following tutorial by Stephen Diehl. 
(The language does not yet support branching and mutable variables)

[![Build Status](https://travis-ci.org/sdiehl/kaleidoscope.svg)](https://travis-ci.org/sdiehl/kaleidoscope)
[![MIT License](http://img.shields.io/badge/license-mit-blue.svg)](https://github.com/sdiehl/kaleidoscope/blob/master/LICENSE-MIT)

Read Online:

* [**HTML**](http://www.stephendiehl.com/llvm)
* [**PDF**](http://www.stephendiehl.com/llvm/tutorial.pdf)


Setup
-----

You will need GHC 7.8 or newer as well as LLVM 9.0. For information on installing LLVM 9.0 

With Haskell and LLVM in place, you can use either Stack or Cabal to install the necessary Haskell
bindings and compile the source code from each chapter.

### Building with Stack (Recommended)

```bash
$ stack build
```

You can then run the source code from each chapter (starting with chapter 2) as follows:

```bash
$ stack exec quick
```

License
-------

Text is adapted from the LLVM tutorial and is subsequently licensed under the
LLVM license.

The Haskell source files are released under the MIT license. Copyright (c)
2013-2016, Stephen Diehl
