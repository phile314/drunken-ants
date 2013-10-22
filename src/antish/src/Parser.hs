module Parser (
  parseStr, parseFile, loadImports, Loader,
  module Text.Parsec.Error
  ) where

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Ast
import Text.Parsec.Error
import Parser.Program
import Control.Monad
import Control.Monad.Error
import Data.Either

-- | Builds an abstract syntax tree from the given program. Input not
--   adhering to the high-level ant grammar is refused and an error
--   returned, but no further checking is done.
parseStr :: String -> Maybe String -> Loader Program
parseStr inp (Just file) = return (parse pProgram inp file) >>= either throwError return 
parseStr inp Nothing     = return (parse pProgram inp ""  ) >>= either throwError return

-- | Does the same thing as `parseStr`, but reads its
--   input from a file.
parseFile :: String -> Loader Program
parseFile f = do 
  liftIO (parseFromFile pProgram f) >>= either throwError return

-- | Recursively parses all the modules in a file and returns
-- the ordered list of bindings

type Loader = ErrorT ParseError IO

instance Error ParseError where
  strMsg = undefined

loadImports :: [String] -- Names of the modules (without extension)
            -> Loader [Binding]
loadImports moduleNames = do
  ps <- forM moduleNames (\name -> parseFile (name ++ ".ha"))
  res <- forM ps (\(Program i t) -> liftM (t ++) (loadImports i)) -- TODO check again position of t
  return $ concat res
