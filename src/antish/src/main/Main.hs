module Main where

import Parser
import System.Environment
import Ast
import Options.Applicative
import Control.Monad.Error

data Options = Options
  { showAST :: Bool
  , srcFile :: String }

-- TODO
options :: Parser Options
options = Options
  <$> switch ( long "show-ast" <> short 's' <> help "Show generated AST Tree." )
  <*> argument str ( metavar "SRC" )


run :: Options -> Loader Program
run opts = do
  Program imports top <- parseFile (srcFile opts)
  res <- loadImports imports
  return $ Program imports (res ++ top)

main :: IO ()
main = do
  options <- execParser opts 
  res <- runErrorT (run options)
  case res of
    Right r -> print r
    Left e -> print e
  where
    opts = info (helper <*> options)
      ( fullDesc <> progDesc "Compiles High-level Ant Code to Low-level Ant Code.")

