llvm-hs-pretty
--------------

[![Build Status](https://travis-ci.org/llvm-hs/llvm-hs-pretty.svg)](https://travis-ci.org/llvm-hs/llvm-hs-pretty)
[![Hackage](https://img.shields.io/hackage/v/llvm-hs-pretty.svg)](https://hackage.haskell.org/package/llvm-hs-pretty)

A pretty printer for ``llvm-hs-pure``. The goal of this project is to be able
to pretty print a sufficiently large subset of the LLVM AST from pure Haskell
without having to go through the C++ API.

> **Note**: It is possible to construct well-typed Haskell ASTs that are
> nevertheless considered invalid by LLVM, and as such there is no meaningful way
> to print them. Always run the LLVM verifier on your AST to test it is sound. If
> you encounter incomplete pattern matches using this library you likely have
> constructed invalid IR.

Usage
-----

There is a single function ``ppllvm`` that maps a `LLVM.AST.Module` to a `Text`.

```haskell
import LLVM.AST
import LLVM.Pretty (ppllvm)

ppllvm :: MonadModuleBuilder m => Module -> m Text
```

Individual LLVM IR elements can be printed using the the polymorphic ``ppll``
function for any LLVM entity that implements the ``Pretty`` typeclass.

Note that many LLVM entities are printed with types inline. For example, an add
instruction operating on 32-bit integers `a` and `b` is printed as `add i32 %a,
%b`. Since LLVM named type definitions can only be resolved by querying the
module state to find the referent type, pretty-printing an entity with a
displayed type involves a stateful type-lookup operation to compute what that
type should be. These entities cannot have a stateless `Pretty` instance, but
instead have dedicated stateful pretty printing primitives. For example, to
pretty print an instruction, you can use `ppInstruction`:

```
ppInstruction :: MonadModuleBuilder m => Instruction -> m (Doc ann)
```

The function `renderll :: Doc ann -> Text` is provided for rendering the output
of any of these primitives to a `Text`.

Tests
-----

The test suite currently consists of round tripping LLVM IR from correct IR
outputted by the LLVM toolchain, parsing into llvm-hs AST and then printing it
back out and comparing it with the original textual form to see if the pretty
printer faithfully preserves the structure. The sample modules are in
``tests/``.

Using stack:

```bash
$ stack build
$ stack test
```

Using cabal:

```bash
$ cabal run
$ cabal run -- tests/simple.ll
```

If you're using Nix then:

```bash
$ nix-shell
$ cabal run
```

Example
-------

To try out the standalone example run:

```bash
$ stack exec -- Example
```

License
-------

Released under the MIT License.

Copyright (c) 2014-2020, Stephen Diehl
Copyright (c) 2015 Cedric Shock
