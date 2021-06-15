{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Pretty Printer
import LLVM.Pretty

-- AST
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global

-- Builders
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad hiding (block)

import Data.Text.Lazy.IO as TIO
import Control.Monad

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
  let (prettyModule, prettyDef, prettyBlock) = fst $ runModuleBuilder emptyModuleBuilder $ do
                                                 mapM_ emitDefn $ moduleDefinitions astModule
                                                 prettyModule <- ppllvm astModule
                                                 prettyDef <- liftM renderll $ ppDefinition defAdd
                                                 prettyBlock <- liftM renderll $ ppBasicBlock block
                                                 return (prettyModule, prettyDef, prettyBlock)

  TIO.putStrLn "=== Module ==="
  TIO.putStrLn prettyModule

  TIO.putStrLn "=== Definition ==="
  TIO.putStrLn prettyDef

  TIO.putStrLn "=== Basic Block ==="
  TIO.putStrLn prettyBlock
