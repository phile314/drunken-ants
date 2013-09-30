module Parser.Program where

import Control.Applicative
import Text.Parsec.String
import Text.Parsec.Combinator
import Ast
import Parser
import Parser.Boolean

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

