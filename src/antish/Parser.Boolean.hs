module  Parser.Boolean where

import Ast ( BoolExpr (..), SenseDir (..), Cond (..) )
import Control.Applicative
import Text.Parsec.String
import Text.Parsec.Combinator
import Parser

pBoolExpr :: GenParser Char st BoolExpr
pBoolExpr = (pNot <|> pOr <|> pAnd <|> pCondition) <|> parens pBoolExpr
  
pNot :: GenParser Char st BoolExpr
pNot = Not <$> (reservedOp "!" *> pBoolExpr)

pOr :: GenParser Char st BoolExpr
pOr = Or <$> pBoolExpr <*> (reservedOp "||" *> pBoolExpr)

pAnd :: GenParser Char st BoolExpr 
pAnd = And <$> pBoolExpr <*> (reservedOp "&&" *> pBoolExpr)

pCondition :: GenParser Char st BoolExpr
pCondition = Condition <$> pCond <*> pSenseDir

pCond :: GenParser Char st Cond
pCond = choice conditions
  where conditions = [pFriend, pFoe, pFriendWithFood, pFoeWithFood,
                      pFood, pRock, pFoeMarker, pHome, pFoeHome ]

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
pMarker = Marker <$> (reserved "Marker" *> natural)   -- TODO should be check now that number in [0,5]

pSenseDir :: GenParser Char st SenseDir
pSenseDir = undefined
