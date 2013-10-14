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


parseStr :: String -> Maybe String -> Either ParseError Program
parseStr inp (Just file) = parse pProgram inp file
parseStr inp Nothing     = parse pProgram inp ""

parseFile :: String -> IO (Either ParseError Program)
parseFile = parseFromFile pProgram
