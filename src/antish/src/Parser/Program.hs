module Parser.Program (
    module Parser.Program -- developing - TODO remove
  , module Parser.Boolean -- developing - TODO remove
  , module Text.Parsec.Prim )
  where

import Control.Applicative hiding ((<|>))
import Text.Parsec.Prim
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
pExpr = pInt 

pInt :: GenParser Char st Expr
pInt = ConstInt <$> natural

pBinding :: GenParser Char st Binding
pBinding = try pVarDecl <|> pFunDecl

pFunDecl :: GenParser Char st Binding
pFunDecl = undefined 

pVarDecl :: GenParser Char st Binding
pVarDecl = undefined
