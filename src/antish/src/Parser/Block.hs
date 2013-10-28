module Parser.Block where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Prim
import Ast
import Parser.LangDef
import Parser.Expr
import Parser.Builtin
import Text.Parsec.Combinator
import Data.Maybe

pStmBlock :: GenParser Char st StmBlock
pStmBlock = StmBlock <$> braces (many1 pStatement)

pStatement :: GenParser Char st Statement
pStatement = choice $ builtin ++ [pIfThenElse, pLet, pFor, pTry, pProp, pFunCall]

pIfThenElse :: GenParser Char st Statement
pIfThenElse = IfThenElse <$> pIf <*> pThen <*> pElse
  where pIf   = reserved "if" *> pBoolExpr 
        pThen = reserved "then" *> pStmBlock 
        pElse = (reserved "else" *> pStmBlock) <|> empty
        empty = pure $ StmBlock []

pLet :: GenParser Char st Statement 
pLet = Let <$> bindings <*> block
  where bindings = reserved "let" *> pBinding `sepBy1` comma
        block    = reserved "in" *> pStmBlock

pBinding :: GenParser Char st Binding
pBinding = try pVarDecl <|> pFunDecl

pFunDecl :: GenParser Char st Binding
pFunDecl = FunDecl <$> rec <*> identifier <*> params <*> body 
  where rec = maybe NonRec (const Rec) <$> optionMaybe (reserved "rec")
        params = many identifier
        body = reserved "=" *> pStmBlock

pVarDecl :: GenParser Char st Binding
pVarDecl = VarDecl <$> identifier <*> (reserved "=" *> pExpr) 

pFunCall :: GenParser Char st Statement
pFunCall = FunCall <$> identifier <*> many pExpr <* semi

pFor :: GenParser Char st Statement
pFor = For <$> pFor <*> list <*> pStmBlock
  where list = brackets $ pExpr `sepBy` comma
        iterVar = identifier <* reserved "in"
        pFor = reserved "for" *> optionMaybe iterVar

pTry :: GenParser Char st Statement
pTry = Try <$> tryBlock <*> catchBlock
  where tryBlock   = reserved "try" *> pStmBlock
        catchBlock = reserved "catch" *> pStmBlock

pProp :: GenParser Char st Statement
pProp = WithProb <$> withprob <*> block1 <*> block2
  where withprob = reserved "with" *> reserved "probability" *> float
        block1 = reserved "do" *> pStmBlock
        block2 = reserved "otherwise" *> pStmBlock
