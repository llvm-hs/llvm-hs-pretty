llvm-hs-pretty
--------------

[![Build Status](https://travis-ci.org/llvm-hs/llvm-hs-pretty.svg)](https://travis-ci.org/llvm-hs/llvm-hs-pretty)
[![Hackage](https://img.shields.io/hackage/v/llvm-hs-pretty.svg)](https://hackage.haskell.org/package/llvm-hs-pretty)

A pretty printer for ``llvm-hs-pure``. Goal is to be able to pretty print a
sufficiently large subset of the LLVM AST from pure Haskell without having to go
through the C++ API.

> **Note**: It is possible to construct llvm-hs-pure ASTs that are invalid ASTs
> and as such there is no meaningful way to print them. Always run the LLVM
> verifier on your AST to test it is sound. If you encounter incomplete pattern
> matches using this library you likely have constructed invalid IR.

Usage
-----

There is a single function ``ppllvm`` that maps a LLVM.AST.Module to a Text.

```haskell
import LLVM.AST
import LLVM.Pretty (ppllvm)

ppllvm :: Module -> Text
```

Individual LLVM elements (constants, instructions) can be printed using the
the polymorphic ``ppll`` function for any LLVM structure that implements the
``PP`` typeclass.

Tests
-----

```bash
# This is only necessary for running the test suite
sudo apt-get install llvm-9-dev
```

The test suite currently consists of round tripping a LLVM IR from correct IR
outputted by the llc toolchain, parsing into llvm-hs AST and then printing it
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
$ stack repl
$ :load Example.hs
main
```

Consider the basic example LLVM module.

```llvm
; ModuleID = 'example-llvm-module'

define i8 @f(i8 %x){
entry:
  ret i8 %x
}
```

Using the LLVM.AST we construct the type and feed it to the pretty
printer.

```haskell
module Standalone where

-- Pretty Printer
import LLVM.Pretty (ppllvm)

-- AST
import qualified LLVM.AST as AST
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Visibility as Visibility
import qualified LLVM.AST.CallingConvention as Convention

import Data.Text.Lazy.IO as TIO

astModule :: AST.Module
astModule = AST.Module
    { AST.moduleName         = "example-llvm-module"
    , AST.moduleDataLayout   = Nothing
    , AST.moduleTargetTriple = Nothing
    , AST.moduleDefinitions  =
        [ AST.GlobalDefinition
            (AST.Function
                Linkage.External
                Visibility.Default
                Nothing
                Convention.C
                []
                (AST.IntegerType 8)
                (AST.Name "f")
                ([AST.Parameter (AST.IntegerType 8) (AST.Name "x") []], False)
                []
                Nothing
                Nothing
                0
                Nothing
                Nothing
                [ AST.BasicBlock
                    (AST.Name "entry")
                    []
                    (AST.Do
                        (AST.Ret
                            (Just
                                (AST.LocalReference
                                    (AST.IntegerType 8)
                                    (AST.Name "x")
                                )
                            )
                            []
                        )
                    )
                ]
            )
        ]
    }

main :: IO ()
main = TIO.putStrLn (ppllvm astModule)
```

License
-------

Released under the MIT License.

Copyright (c) 2014-2020, Stephen Diehl
Copyright (c) 2015 Cedric Shock
