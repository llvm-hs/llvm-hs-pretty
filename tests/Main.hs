module Main where

import LLVM.Context
import LLVM.Pretty (ppllvm)
import LLVM.AST
import qualified LLVM.Module as M
import LLVM.IRBuilder.Module

import Control.Monad (filterM)
import Control.Monad.Except

import Data.Functor
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import System.IO
import System.Exit
import System.Directory
import System.FilePath
import System.Environment

import Test.Tasty
import Test.Tasty.HUnit

-------------------------------------------------------------------------------
-- Harness
-------------------------------------------------------------------------------

llvmFile :: FilePath -> IO Bool
llvmFile fname = do
  str <- readFile fname
  withContext $ \ctx -> do
    M.withModuleFromLLVMAssembly ctx str $ \mod -> do
      ast <- M.moduleAST mod
      let str' = fst $ runModuleBuilder emptyModuleBuilder $ do
                  mapM emitDefn $ moduleDefinitions ast
                  ppllvm ast
      T.writeFile ("tests/output" </> takeFileName fname) str'
      M.withModuleFromLLVMAssembly ctx (T.unpack str') $ \mod' -> do
        ast' <- M.moduleAST mod'
        return True

makeTest :: FilePath -> TestTree
makeTest fname = testCase fname $ assertBool "" =<< llvmFile fname

testPath :: FilePath
testPath = "tests/input/"

suite :: IO TestTree
suite = do
  dirFiles <- listDirectory testPath
  createDirectoryIfMissing True "tests/output"
  let testFiles = fmap (\x -> testPath </> x) dirFiles
  pure $ testGroup "Test Suite" [
    testGroup "Roundtrip Tests" $ fmap makeTest testFiles
    ]

main :: IO ()
main = defaultMain =<< suite
