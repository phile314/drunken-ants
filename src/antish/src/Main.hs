-- | This module defines the entry point for the compiler.

module Main where

import Parser
import System.Environment
import Ast
import Options.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Compiler
import Code
import Tree

-- | Accepted command line parameters.
data Options = Options
  { showAST :: Bool
  , srcFile :: FilePath }


options :: Parser Options
options = Options
  <$> switch ( long "show-ast" <> short 's' <> help "Show generated AST Tree." )
  <*> argument str ( metavar "SRC" )

-- | Parses the file and load the imported modules if any.
-- If the program (and the modules) are syntactically correct
-- the resulting abstract syntax tree is returned, otherwise
-- an error is returned.
loadFiles :: String -> Loader Program
loadFiles srcFile = do
  Program imports top <- parseFile srcFile
  res <- loadImports imports
  return $ Program imports (res ++ top)

run :: Options -> IO ()
run opts = do
  p <- runErrorT $ loadFiles (srcFile opts)

  p' <- case p of
            (Left e) -> error (show e)
            (Right k) -> do
              when (showAST opts) $ putStrLn (drawAst k)
              return k

  p'' <- case (compile p') of
        (Left e) -> error (show e)
        (Right k) -> return k
 
  putStr $ toCode p''

  return ()

-- | The entry point for the compiler.
main :: IO ()
main = do
  options <- execParser opts
  run options
  where
    opts = info (helper <*> options)
      ( fullDesc <> progDesc "Compiles High-level Ant Code to Low-level Ant Code.")

