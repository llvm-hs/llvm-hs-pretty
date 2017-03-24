llvm-pp
-------

[![Build Status](https://travis-ci.org/sdiehl/llvm-pp.svg)](https://travis-ci.org/sdiehl/llvm-pp)

A pretty printer for ``llvm-general-pure``. Goal is to be able to pretty print a
sufficiently large subset of the LLVM AST from pure Haskell without having to go
through the C++ API.

Pretty much no way this code is going to pretty, it's a giant string munging
program. Be warned.

Usage
-----

```bash
sudo apt-get install llvm-dev-3.8
```

There is a single function ``ppllvm`` that maps a LLVM.General.AST.Module to a
String.

```haskell
import LLVM.General.AST
import LLVM.General.Pretty (ppllvm)

ppllvm :: Module -> Text
```

Tests
-----

The test suite currently consists of round tripping a LLVM IR from correct IR
outputted by the llc toolchain, parsing into llvm-general AST and then printing
it back out and comparing it with the original textual form to see if the pretty
printer faithfully preserves the structure. The sample modules are in
``tests/``.

Using stack:

```bash
$ stac build
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

Standalone Example
------------------

To try out the standalone example run:

```bash
$ stack repl
$ :load Standalone.hs
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

Using the LLVM.General AST we construct the type and feed it to the pretty
printer.

```haskell
module Standalone where

-- Pretty Printer
import LLVM.General.Pretty (ppllvm)

-- AST
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Linkage as Linkage
import qualified LLVM.General.AST.Visibility as Visibility
import qualified LLVM.General.AST.CallingConvention as Convention

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

Copyright (c) 2014-2017, Stephen Diehl
Copyright (c) 2015 Cedric Shock
