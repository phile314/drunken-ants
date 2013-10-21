module Parser.Block where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Prim
import Ast
import Parser.LangDef
import Parser.Boolean
import Text.Parsec.Combinator

pStmBlock :: GenParser Char st Statement
pStmBlock = StmBlock <$> (moreStatements <|> oneStatement)
  where moreStatements = braces $ pStatement `sepBy1` semi 
        oneStatement   = count 1 pStatement

pStatement :: GenParser Char st Statement
pStatement = choice $ map try [pIfThenElse, pLet, pFor, pTry, pFunCall]

pIfThenElse :: GenParser Char st Statement
pIfThenElse = IfThenElse <$> (reserved "if" *> pBoolExpr) <*> 
                             (reserved "then" *> pStmBlock) <*> 
                             ((reserved "else" *> pStmBlock) <|> (pure (StmBlock [])))

pExpr :: GenParser Char st Expr
pExpr = pInt 

pInt :: GenParser Char st Expr
pInt = ConstInt . fromIntegral <$> natural

pLet :: GenParser Char st Statement 
pLet = Let <$> (reserved "let" *> endLineSep pBinding) <*> (reserved "in" *> pStmBlock)
  where endLineSep = flip sepBy1 comma

pBinding :: GenParser Char st Binding
pBinding = try pVarDecl <|> pFunDecl

pFunDecl :: GenParser Char st Binding
pFunDecl = FunDecl <$> identifier <*> many identifier <*> (reserved "=" *> pStmBlock)

pVarDecl :: GenParser Char st Binding
pVarDecl = VarDecl <$> identifier <*> (reserved "=" *> pExpr) 

-- Additional checks: 
-- * does the function exists?
-- * it is called with the correct number of parameters?
pFunCall :: GenParser Char st Statement
pFunCall = FunCall <$> identifier <*> many pExpr

pFor :: GenParser Char st Statement
pFor = For <$> (reserved "for" *> optionMaybe iterVar) <*> list <*> pStmBlock
  where list = brackets $ pInt `sepBy` comma    -- TODO point free style -> general purpose function
        iterVar = identifier <* reserved "in"

pTry :: GenParser Char st Statement
pTry = Try <$> (reserved "try" *> pStmBlock)  <*> 
               (reserved "catch" *> pStmBlock)


{- -- Working parser with improved syntax, in the AS the binding has for this to be changed

-- | parse multiple statements
pStmBlock :: GenParser Char st StmBlock
pStmBlock = StmBlock <$> many pStatement

-- | parse a statement
pStatement :: GenParser Char st Statement
pStatement = pIfThenElse <|> pLetDecl <|> pLetFunc <|> pFor <|> pTry <|> pFunCall

-- | parse a if statement
pIfThenElse :: GenParser Char st Statement
pIfThenElse = IfThenElse <$> (reserved "if" *> pBoolExpr) <*> 
                             (reserved "{" *> pStmBlock <* reserved "}") <*> 
                             optionMaybe (reserved "else" *> reserved "{" *> pStmBlock <* reserved "}") 

-- | parse an expression
pExpr :: GenParser Char st Expr
pExpr = pInt 

-- | parse a natural number
pInt :: GenParser Char st Expr
pInt = ConstInt <$> natural

-- | parse a decleration
pLetDecl = Let <$> (reserved "let" *> pVarDecl <* semi) -- here could a list be parsed

-- | parse a function statement
pLetFunc = Let <$> (reserved "def" *> pFunDecl)

pFunDecl :: GenParser Char st Binding
pFunDecl = FunDecl <$> identifier <*> (reserved "{" *> pStmBlock <* reserved "}")

pVarDecl :: GenParser Char st Binding
pVarDecl = VarDecl <$> identifier <*> (reserved "=" *> pExpr)

-- Additional checks: 
-- * does the function exists?
-- * it is called with the correct number of parameters?
pFunCall :: GenParser Char st Statement
pFunCall = FunCall <$> identifier <*> many pExpr <* semi

-- | parse a for statement
pFor :: GenParser Char st Statement
pFor = For <$> (reserved "for" *> optionMaybe iterVar) <*> list <*> pStmBlock
  where list = brackets $ pInt `sepBy` comma    -- TODO point free style -> general purpose function
        iterVar = identifier <* reserved "in"

-- | parse a try statement
pTry :: GenParser Char st Statement
pTry = Try <$> (reserved "try" *> reserved "{" *> pStmBlock <* reserved "}")  <*> 
               (reserved "with" *> reserved "{" *> pStmBlock <* reserved "}") <*> 
               (reserved "catch" *> reserved "{" *> pStmBlock <* reserved "}")
-}
