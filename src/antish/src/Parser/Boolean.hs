-- | This module provides parsers for boolean expressions

module  Parser.Boolean where

import Ast ( BoolExpr (..), Cond (..), SenseDir(..))
import Control.Applicative
import Text.Parsec.String
import Text.Parsec.Combinator 
import Parser.LangDef
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Expr
import Text.Read hiding (parens, choice)

-- | Parses a (parenthesized) boolean expression
pBoolExpr :: GenParser Char st BoolExpr
pBoolExpr    = buildExpressionParser bOperators bTerm

-- | A list defining all the boolean operators, their associativity and prefix - infix - postfix use
bOperators = [ [ Prefix (reservedOp "!" >> return Not) ] 
             , [ Infix (reservedOp "&&" >> return And) AssocLeft ]
             , [ Infix (reservedOp "||" >> return Or) AssocLeft ]
             ]

-- | Parses a simple condition made by 'Cond' 'Sensedir'
bTerm :: GenParser Char st BoolExpr
bTerm = Condition <$> pCond <*> pSenseDir <|> parens pBoolExpr

-- | Parses a 'SenseDir' constant
pSenseDir :: GenParser Char st SenseDir
pSenseDir = choice $ map makeConstParser [Here, Ahead, LeftAhead, RightAhead]

-- | Parses a 'Cond' constant
pCond :: GenParser Char st Cond
pCond = choice (pMarker:conditions)
  where conditions = map makeConstParser [Friend, Foe, FriendWithFood, FoeWithFood,
                                          Food, Rock, FoeMarker, Home, FoeHome]

pMarker :: GenParser Char st Cond
pMarker = Marker <$> (reserved "Marker" *> natural)

-- | @makeConstParser c@ creates a parser for the constructor without arguments @c@
-- of a showable type.
makeConstParser :: (Show a) => a -> GenParser Char st a
makeConstParser x = reserved (show x) *> return x 
