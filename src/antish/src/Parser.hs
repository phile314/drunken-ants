module Parser (
  parseStr, parseFile,
  module Text.Parsec.Error
  ) where

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Ast
import Text.Parsec.Error
import Parser.Program


-- | Builds an abstract syntax tree from the given program. Input not
--   adhering to the high-level ant grammar is refused and an error
--   returned, but no further checking is done.
parseStr :: String -> Maybe String -> Either ParseError Program
parseStr inp (Just file) = parse pProgram inp file
parseStr inp Nothing     = parse pProgram inp ""

-- | Does the same thing as `parseStr`, but reads its
--   input from a file.
parseFile :: String -> IO (Either ParseError Program)
parseFile = parseFromFile pProgram
