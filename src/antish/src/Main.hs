-- | This module defines the entry point for the compiler.

module Main where

import Parser
import System.Environment
import Ast
import Control.Monad.Error
import Control.Monad.Identity
import Compiler
import Code
import Loader
import Options.Applicative
import System.IO
import System.Exit
import Tree

-- | Accepted command line parameters.
data Options = Options
  { showAST :: Bool       -- ^ (Debugging) Show the abstract syntax tree
  , srcFile :: FilePath   -- ^ The input file
  , outFile :: FilePath   -- ^ The output file
  }


options :: Parser Options
options = Options
  <$> switch ( long "show-ast" <> short 's' <> help "Show generated AST Tree." )
  <*> argument str (metavar "<file>")
  <*> strOption (long "output" <> short 'o' <> metavar "<file>") 

-- | Parser for the command line options
pOpts :: ParserInfo Options
pOpts = info (helper <*> options)
      (fullDesc <> progDesc "Compiles High-level Ant Code to Low-level Ant Code.")

-- | Parses the file and load the imported modules if any.
-- If the program (and the modules) are syntactically correct
-- the resulting abstract syntax tree is returned, otherwise
-- an error is returned.
loadFiles :: String -> Loader Program
loadFiles srcFile = do
  Program imports top <- parseFile srcFile
  res <- loadImports imports
  return $ Program imports (res ++ top)

-- | Performs the parsing and the compilation of the input file
-- On failure a 'LError' is raised, otherwise both the program's 
-- abstract syntax tree and the assembly code is returned.
run :: Options -> Loader (Program, [Instruction])
run opts = do
  ast <- loadFiles (srcFile opts)
  case compile ast of
    Left e -> throwError $ C e
    Right p -> return (ast,p)

-- | The entry point for the compiler.
main :: IO ()
main = do
  options <- execParser pOpts
  r <- runErrorT $ run options
  case r of
    Left e -> hPutStrLn stderr (show e) >> exitFailure
    Right (ast, p) -> do
      when (showAST options) $ putStrLn (drawAst ast)
      writeFile (outFile options) $ toCode p
