module Parser.Program (
    module Parser.Program -- developing - TODO remove
  , module Text.Parsec.Prim )
  where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Combinator
import Ast
import Parser
import Parser.Block

pProgram :: GenParser Char st Program
pProgram = Program <$> pStmBlock
