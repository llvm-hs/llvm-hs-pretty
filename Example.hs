{-# LANGUAGE OverloadedStrings #-}

module Example where

-- Pretty Printer
import LLVM.Pretty (ppllvm, ppll)

-- AST
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global

import Data.Text.Lazy.IO as TIO

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [block]
  }
  where

block :: BasicBlock
block = BasicBlock
    (Name "entry")
    [ Name "result" :=
        Add False
            False
            (LocalReference int (Name "a"))
            (LocalReference int (Name "b"))
            [] ]
    (Do $ Ret (Just (LocalReference int (Name "result"))) [])

astModule :: AST.Module
astModule = defaultModule
  { moduleName = "llvm-pp"
  , moduleDefinitions = [defAdd]
  }

main :: IO ()
main = do
  TIO.putStrLn "=== Module ==="
  TIO.putStrLn (ppllvm astModule)

  TIO.putStrLn "=== Definition ==="
  TIO.putStrLn (ppll defAdd)

  TIO.putStrLn "=== Basic Block ==="
  TIO.putStrLn (ppll block)
