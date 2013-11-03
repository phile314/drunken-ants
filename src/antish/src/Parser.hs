-- | This module defines some high-level function for parsing a program

module Parser (
  parseFile, loadImports, Loader,
  module Text.Parsec.Error
  ) where

import Text.Parsec.String
import Ast
import Text.Parsec.Error
import Parser.Program
import Control.Monad
import Control.Monad.Error
import Loader

-- | @'parseFile' filepath@ parses the given @file@ and match it against the
-- language grammar.
parseFile :: FilePath -> Loader Program
parseFile f = do 
  liftIO (parseFromFile pProgram f) >>= either (throwError . P) return

-- | Recursively parsess all the modules imported and returns
-- the ordered list of bindings.
loadImports :: [String] -- Names of the modules to import (without extension)
            -> Loader [Binding]
loadImports moduleNames = do
  ps <- forM moduleNames (\name -> parseFile (name ++ ".ha"))
  res <- forM ps (\(Program i t) -> liftM (++ t) (loadImports i))
  return $ concat res
