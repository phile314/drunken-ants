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

bOperators = [  [Prefix (reservedOp "!"   *> return (Not              ))          ]
              , [Infix  (reservedOp "&&"  *> return (And              )) AssocLeft]
              , [Infix  (reservedOp "||"  *> return (Or               )) AssocLeft]
              ]

bTerm :: GenParser Char st BoolExpr
bTerm = term <|> parens term
  where term = Condition <$> pCond <*> pSenseDir 

pBoolExpr :: GenParser Char st BoolExpr
pBoolExpr = buildExpressionParser bOperators bTerm

-------------------------------------------------------------------------------
-- SenseDir Parsers

pSenseDir :: GenParser Char st SenseDir
pSenseDir = choice directions
  where directions = [pHere, pAhead, pLeftAhead, pRightAhead]

pHere :: GenParser Char st SenseDir
pHere = reserved "Here" *> return Here

pAhead :: GenParser Char st SenseDir
pAhead = reserved "Ahead" *> return Ahead

pLeftAhead :: GenParser Char st SenseDir
pLeftAhead = reserved "LeftAhead" *> return LeftAhead

pRightAhead :: GenParser Char st SenseDir
pRightAhead = reserved "RightAhead" *> return RightAhead

-------------------------------------------------------------------------------
-- Cond Parsers

-- TODO use map on pairs for pCond
-- map magic [(Foe, "Foe"), ... ]

pCond :: GenParser Char st Cond
pCond = choice conditions
  where conditions = [pFriend, pFoe, pFriendWithFood, pFoeWithFood,
                      pFood, pRock, pFoeMarker, pHome, pFoeHome, pMarker]

pFriend :: GenParser Char st Cond
pFriend = reserved "Friend" *> return Friend

pFoe :: GenParser Char st Cond
pFoe = reserved "Foe" *> return Foe

pFriendWithFood :: GenParser Char st Cond
pFriendWithFood = reserved "FriendWithFood" *> return FriendWithFood

pFoeWithFood :: GenParser Char st Cond
pFoeWithFood = reserved "FoeWithFood" *> return FoeWithFood

pFood :: GenParser Char st Cond
pFood = reserved "Food" *> return Food

pRock :: GenParser Char st Cond
pRock = reserved "Rock" *> return Rock

pFoeMarker :: GenParser Char st Cond
pFoeMarker = reserved "FoeMarker" *> return FoeMarker

pHome :: GenParser Char st Cond
pHome = reserved "Home" *> return Home

pFoeHome :: GenParser Char st Cond
pFoeHome = reserved "FoeHome" *> return FoeHome

pMarker :: GenParser Char st Cond
pMarker = Marker <$> (reserved "Marker" *> natural)   -- TODO should we check now that number in [0,5]?

