module Main where

import Parser
import System.Environment
import Ast
import Options.Applicative
import Control.Monad.Error
import Simplify
import Control.Monad.Identity
import Compiler

data Options = Options
  { showAST :: Bool
  , srcFile :: String }

-- TODO
options :: Parser Options
options = Options
  <$> switch ( long "show-ast" <> short 's' <> help "Show generated AST Tree." )
  <*> argument str ( metavar "SRC" )

loadFiles :: String -> Loader Program
loadFiles srcFile = do
  Program imports top <- parseFile srcFile
  res <- loadImports imports
  let prog = Program imports (res ++ top)
  return prog

run :: Options -> IO ()
run opts = do
  p <- runErrorT $ loadFiles (srcFile opts)

  p' <- case p of
            (Left e) -> error (show e)
            (Right k) -> do
              when (showAST opts) $ print p
              return k

  {-p'' <- case (simplify p') of
              (Left e) -> error (show e)
              (Right k) -> do
                when (showAST opts) $ print k
                return k-}
  p'' <- case (compile p') of
        (Left e) -> error (show e)
        (Right k) -> return k
 
  mapM_ (putStrLn . show) p''

  return ()

main :: IO ()
main = do
  options <- execParser opts
  run options
  where
    opts = info (helper <*> options)
      ( fullDesc <> progDesc "Compiles High-level Ant Code to Low-level Ant Code.")

