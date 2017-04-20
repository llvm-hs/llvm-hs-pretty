module Main where

import qualified LLVM.Module as M
import LLVM.Context
import LLVM.Pretty (ppllvm)

import Control.Monad (filterM)
import Control.Monad.Except
import Control.Exception (try, SomeException)

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
    res <- try $ M.withModuleFromLLVMAssembly ctx str $ \mod -> do
      ast <- M.moduleAST mod
      putStrLn $ ppShow ast
      let str = ppllvm ast
      T.putStrLn str
      T.writeFile ("tests/output" </> takeFileName fname) str
      trip <- try $ M.withModuleFromLLVMAssembly ctx (T.unpack str) (const $ return ())
      case trip of
        Left err -> do
          putStrLn "Error reading output:"
          let err' = show (err :: SomeException)
          putStrLn err'
          writeFile ("tests/output" </> takeFileName fname) err'
          exitFailure
        Right ast -> putStrLn "Round Tripped!"

    case res of
      Left err -> do
        putStrLn "Error reading input:"
        let err' = show (err :: SomeException)
        putStrLn err'
        writeFile ("tests/output" </> takeFileName fname) err'
        exitFailure
      Right _ -> return ()

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
