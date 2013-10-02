module Parser.Program (
    module Parser.Program -- developing - TODO remove
  , module Parser.Boolean -- developing - TODO remove
  , module Text.Parsec.Prim )
  where

import Control.Applicative hiding ((<|>), many)
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
pStatement = pIfThenElse <|> pLet

pIfThenElse :: GenParser Char st Statement
pIfThenElse = IfThenElse <$> (reserved "if" *> pBoolExpr) <*> 
                             (reserved "then" *> pStmBlock) <*> 
                             (reserved "else" *> pStmBlock) 

pExpr :: GenParser Char st Expr
pExpr = pInt 

pInt :: GenParser Char st Expr
pInt = ConstInt <$> natural

pLet :: GenParser Char st Statement 
pLet = Let <$> (reserved "let" *> endLineSep pBinding) <*> (reserved "in" *> pStmBlock)
  where endLineSep = flip sepBy1 comma

pBinding :: GenParser Char st Binding
pBinding = try pVarDecl <|> pFunDecl

pFunDecl :: GenParser Char st Binding
pFunDecl = FunDecl <$> pFIdent <*> many identifier <*> (reserved "=" *> pStmBlock)

pVarDecl :: GenParser Char st Binding
pVarDecl = VarDecl <$> identifier <*> (reserved "=" *> pExpr) 

pFIdent :: GenParser Char st FIdent
pFIdent = FIdent <$> identifier  
