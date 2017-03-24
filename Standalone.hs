module Standalone where

-- Pretty Printer
import LLVM.General.Pretty (ppllvm)

-- AST
import LLVM.General.AST
import qualified LLVM.General.AST as AST
import LLVM.General.AST.Global

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
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False
                False
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])

astModule :: AST.Module
astModule = defaultModule
  { moduleName = "llvm-pp"
  , moduleDefinitions = [defAdd]
  }

main :: IO ()
main = TIO.putStrLn (ppllvm astModule)
