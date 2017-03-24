module Main where

import qualified LLVM.General.Module as M
import LLVM.General.Context
import LLVM.General.PrettyPrint
import LLVM.General.Pretty (ppllvm)

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
    res <- runExceptT $ M.withModuleFromLLVMAssembly ctx str $ \mod -> do
      ast <- M.moduleAST mod
      putStrLn $ showPretty ast
      let str = ppllvm ast
      T.putStrLn str
      trip <- runExceptT $ M.withModuleFromLLVMAssembly ctx (T.unpack str) (const $ return ())
      case trip of
        Left err -> do
          print err
          exitFailure
        Right ast -> putStrLn "Round Tripped!"

    case res of
      Left err -> print err
      Right _ -> return ()

main :: IO ()
main = do
  putStrLn "Running test suite:"
  files <- getArgs

  case files of
    [] -> do
      dircontents <- map (combine "tests") <$> getDirectoryContents "tests"
      dirfiles <- filterM doesFileExist dircontents
      mapM readir dirfiles
    _  -> mapM readir files

  putStrLn "All good."
  return ()
