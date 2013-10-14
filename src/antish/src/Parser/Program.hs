module Parser.Program where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Combinator
import Ast
import Parser.Block

pProgram :: GenParser Char st Program
pProgram = Program <$> pStmBlock

