-- | Defines the parser for 'Program'.

module Parser.Program where

import Ast
import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Char
import Text.Parsec.Prim
import Parser.Block
import Parser.LangDef

-- | Parses a program
pProgram :: GenParser Char st Program
pProgram = Program <$> (spaces *> many pImport) <*>  (whites *> pTop)

-- | Parses top level declarations (no let)
pTop :: GenParser Char st [Binding]
pTop = many pBinding

-- | Parses imports statements
pImport :: GenParser Char st Identifier
pImport = reserved "import" *> identifier

