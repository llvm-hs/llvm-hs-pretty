module Main where

import qualified LLVM.Module as M
import LLVM.Context
import LLVM.Pretty (ppllvm)

import Control.Monad (filterM)
import Control.Monad.Except

import Data.Functor
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Show.Pretty (ppShow)

import System.IO
import System.Exit
import System.Directory
import System.FilePath
import System.Environment

-------------------------------------------------------------------------------
-- Harness
-------------------------------------------------------------------------------

readir :: FilePath -> IO ()
readir fname = do
  putStrLn $ "Test: " ++ fname
  putStrLn $ replicate 80 '='
  putStrLn fname
  putStrLn $ replicate 80 '='
  str <- readFile fname
  withContext $ \ctx -> do
    M.withModuleFromLLVMAssembly ctx str $ \mod -> do
      ast <- M.moduleAST mod
      putStrLn $ ppShow ast
      let str = ppllvm ast
      T.putStrLn str
      T.writeFile ("tests/output" </> takeFileName fname) str
      M.withModuleFromLLVMAssembly ctx (T.unpack str) (const $ return ())

main :: IO ()
main = do
  files <- getArgs

  case files of
    [] -> do
      let testPath = "tests/input/"
      dirFiles <- listDirectory testPath
      mapM_ readir (fmap (\x -> testPath </> x) dirFiles)
    [fpath] -> readir fpath
    _  -> mapM_ readir files

  putStrLn "All good."
  return ()
