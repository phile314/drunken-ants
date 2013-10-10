-- | This module provides parsers for boolean expressions

module  Parser.Boolean (
  module Parser.Boolean -- developing - TODO remove
  )where

import Ast ( BoolExpr (..), Cond (..), SenseDir(..))
import Control.Applicative
import Text.Parsec.String
import Text.Parsec.Combinator 
import Parser
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Expr
import Text.Read hiding (parens, choice)

-- | A list defining all the boolean operators, their associativity and prefix - infix - postfix use
bOperators = [  [Prefix (reservedOp "!"   *> return (Not              ))          ]
              , [Infix  (reservedOp "&&"  *> return (And              )) AssocLeft]
              , [Infix  (reservedOp "||"  *> return (Or               )) AssocLeft]
              ]

-- | Parses a simple condition made by 'Cond' 'Sensedir'
bTerm :: GenParser Char st BoolExpr
bTerm = Condition <$> pCond <*> pSenseDir 

-- | Parses a (parenthesized) boolean expression
-- WARNING parentheses are not always handed properly
pBoolExpr :: GenParser Char st BoolExpr
pBoolExpr = parens exprParser <|> exprParser 
  where exprParser = buildExpressionParser bOperators bTerm

-- | @makeConstParser c@ creates a parser for the constructor without arguments @c@
-- of a showable type.
makeConstParser :: (Show a) => a -> GenParser Char st a
makeConstParser x = reserved (show x) *> return x 

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
