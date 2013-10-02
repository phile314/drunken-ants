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
pStmBlock = StmBlock <$> braces (many1 pStatement)

pStatement :: GenParser Char st Statement
pStatement = pIfThenElse <|> pLet <|> pFunCall <|> pFor

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

-- Additional checks: 
-- * does the function exists?
-- * it is called with the correct number of parameters?
pFunCall :: GenParser Char st Statement
pFunCall = FunCall <$> pFIdent <*> many pExpr

pFor :: GenParser Char st Statement
pFor = For <$> (reserved "for" *> optionMaybe iterVar) <*> list <*> pStmBlock
  where list = brackets $ pInt `sepBy` comma    -- TODO point free style -> general purpose function
        iterVar = identifier <* reserved "in"
