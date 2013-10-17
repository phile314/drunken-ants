module Main where

import Parser
import System.Environment
import Ast
import Options.Applicative


data Options = Options
  { showAST :: Bool
  , srcFile :: String }

-- TODO
options :: Parser Options
options = Options
  <$> switch ( long "show-ast" <> short 's' <> help "Show generated AST Tree." )
  <*> argument str ( metavar "SRC" )


run :: Options -> IO ()
run opts = do
  ast <- parseFile (srcFile opts)

  case ast of
    (Right t) -> print (UseTree t)
    (Left er) -> print er


main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
      ( fullDesc <> progDesc "Compiles High-level Ant Code to Low-level Ant Code.")

  
 

