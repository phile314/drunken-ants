module Parser.Program where

import Ast
import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Char
import Text.Parsec.Prim
import Parser.Block
import Parser.LangDef

pProgram :: GenParser Char st Program
pProgram = Program <$> (spaces *> many pImport) <*>  (whites *> pTop)

-- | Parses top level declarations (no let)
pTop :: GenParser Char st [Binding]
pTop = many pBinding

pImport :: GenParser Char st Identifier
pImport = reserved "import" *> identifier

