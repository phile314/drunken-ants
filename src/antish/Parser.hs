module Parser where

import Control.Applicative
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Ast

antish :: P.LanguageDef st
antish = haskellDef

lexer = P.makeTokenParser antish

reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
natural = P.natural lexer
parens = P.parens lexer

pProgram :: GenParser Char st Program
pProgram = Program <$> pStmBlock

pStmBlock :: GenParser Char st StmBlock
pStmBlock = StmBlock <$> many1 pStatement

pStatement :: GenParser Char st Statement
pStatement = pIfThenElse

pIfThenElse :: GenParser Char st Statement
pIfThenElse = IfThenElse <$> (reserved "if" *> pBoolExpr) <*> 
                             (reserved "then" *> pStmBlock) <*> 
                             (reserved "else" *> pStmBlock) 

pExpr :: GenParser Char st Expr
pExpr = pInt <|> parens pExpr

pInt :: GenParser Char st Expr
pInt = ConstInt <$> natural

pBoolExpr :: GenParser Char st BoolExpr
pBoolExpr = (pNot <|> pOr <|> pAnd) -- <|> parens pBoolExpr
  where pNot = Not <$> (reservedOp "!" *> pExpr)
        pOr = Or <$> pExpr <*> (reservedOp "||" *> pExpr)
        pAnd = And <$> pExpr <*> (reservedOp "&&" *> pExpr) 
