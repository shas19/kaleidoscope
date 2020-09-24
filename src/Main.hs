module Main where

import Parser
import Codegen
import Emit
import JIT

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.AST as AST
import qualified Data.String as STR

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    -- This getInputLine function is from System.Console.Haskeline. They output:: InputT m (Maybe String)
    -- where m is MonadException? what does that mean
    -- why is minput being trated like it is of (Maybe ...) type
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        -- Control.Monad.Trans.liftIO :: Control.Monad.IO.Class.MonadIO m => IO a -> m a
        -- modn <- liftIO $ process mod input
        modn <- liftIO $ process mod input
        case modn of
          --Just modn -> loop modn
          Just modn -> do 
              modn_opt <- liftIO $ runJIT modn
              loop modn_opt
          Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
