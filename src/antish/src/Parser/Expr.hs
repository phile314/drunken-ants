-- | This module provides parsers for expressions

module  Parser.Expr where

import Ast ( Expr (..), Cond (..), SenseDir(..))
import Assembly (LeftOrRight (..))
import Control.Applicative
import Text.Parsec.String
import Text.Parsec.Combinator 
import Parser.LangDef
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Expr
import Text.Read hiding (parens, choice)

pExpr :: GenParser Char st Expr
pExpr = pInt <|> pDir <|> pBoolExpr <|> pVar   -- TODO missing CDir

pInt :: GenParser Char st Expr
pInt = ConstInt . fromIntegral <$> natural

pDir :: GenParser Char st Expr
pDir = left <|> right
  where left  = CDir <$> (reserved "Left" *> pure IsLeft)
        right = CDir <$> (reserved "Right" *> pure IsRight)

pVar :: GenParser Char st Expr
pVar = VarAccess <$> identifier

-------------------------------------------------------------------------------
-- Boolean Expression

-- | Parses a (parenthesized) boolean expression
pBoolExpr :: GenParser Char st Expr
pBoolExpr    = buildExpressionParser bOperators bTerm

-- | A list defining all the boolean operators, their associativity and prefix - infix - postfix use
bOperators = [ [ Prefix (reservedOp "!" >> return Not) ] 
             , [ Infix (reservedOp "&&" >> return And) AssocLeft ]
             , [ Infix (reservedOp "||" >> return Or) AssocLeft ]
             ]

-- | Parses a simple condition made by 'Cond' 'Sensedir'
bTerm :: GenParser Char st Expr
bTerm = Condition <$> pCond <*> pSenseDir <|> pVar <|> parens pBoolExpr

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
