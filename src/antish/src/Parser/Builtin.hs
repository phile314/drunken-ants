-- | This module defines the parsers for builtin constructs, i.e.
-- those functions that can be directly expressed in the assembly language

module Parser.Builtin (builtin) where

import Ast
import Control.Applicative hiding ((<|>), many)
import Parser.Expr
import Parser.LangDef
import Text.Parsec.Combinator
import Text.Parsec.Prim

-- | A list containing the built-in parsers
builtin :: [GenParser Char st Statement]
builtin = [pMark, pUnMark, pTurn, pDrop, pPickUp, pMove]

-- | @'withExpr' s@ parses /s <expr> ;/ and returns the parsed expression.
-- Note that @s@ is considered a reserved word.
withExpr :: String -> GenParser Char st Expr
withExpr s = reserved s *> pExpr <* semi

-- | @'andComma' s@ parses /s ;/.
-- Note that @s@ is considered a reserved word.
withComma :: String -> GenParser Char st ()
withComma s = reserved s <* semi

pMark :: GenParser Char st Statement
pMark =	MarkCall <$> withExpr "Mark"

pUnMark :: GenParser Char st Statement
pUnMark	=	UnMarkCall <$> withExpr "UnMark"

pTurn :: GenParser Char st Statement
pTurn = TurnCall <$> withExpr "Turn"

pDrop :: GenParser Char st Statement
pDrop = withComma "Drop" *> pure DropCall 

pPickUp :: GenParser Char st Statement
pPickUp = withComma "PickUp" *> pure PickUpCall 

pMove :: GenParser Char st Statement
pMove = withComma "Move" *> pure MoveCall 
