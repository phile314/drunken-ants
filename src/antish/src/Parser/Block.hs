module Parser.Block where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Prim
import Ast
import Parser.LangDef
import Parser.Expr
import Text.Parsec.Combinator
import Data.Maybe

pStmBlock :: GenParser Char st StmBlock
pStmBlock = StmBlock <$> (reserved "{" *> many pStatement <* reserved "}")

pStatement :: GenParser Char st Statement
pStatement = stms <|> pIfThenElse <|> pLet <|> pFor <|> pTry <|> pFunCall <|> pProp
	where
		stms = 	MarkCall <$> (reserved "Mark"  *> pExpr <* semi)
			<|>	UnMarkCall <$> (reserved "UnMark"  *> pExpr <* semi)
			<|>	TurnCall <$> (reserved "Turn" *> pExpr <* semi)
			<|>	const DropCall <$> (reserved "Drop" <* semi)
			<|>	const PickUpCall <$> (reserved "PickUp" <* semi)
			<|>	const MoveCall <$> (reserved "Move" <* semi)
			<|>	Label <$> (reserved "Label" *> identifier <* semi)

pIfThenElse :: GenParser Char st Statement
pIfThenElse = IfThenElse <$> (reserved "if" *> pBoolExpr) <*> 
                             (reserved "then" *> pStmBlock) <*> 
                             ((reserved "else" *> pStmBlock) <|> (pure (StmBlock [])))

pLet :: GenParser Char st Statement 
pLet = Let <$> (reserved "let" *> endLineSep pBinding) <*> (reserved "in" *> pStmBlock)
  where endLineSep = flip sepBy1 comma

pBinding :: GenParser Char st Binding
pBinding = try pVarDecl <|> pFunDecl

pFunDecl :: GenParser Char st Binding
pFunDecl = FunDecl <$> rec <*> identifier <*> many identifier <*> (reserved "=" *> pStmBlock)
  where rec = maybe NonRec (const Rec) <$> optionMaybe (reserved "rec")

pVarDecl :: GenParser Char st Binding
pVarDecl = VarDecl <$> identifier <*> (reserved "=" *> pExpr) 

pFunCall :: GenParser Char st Statement
pFunCall = FunCall <$> identifier <*> many pExpr <* semi

pFor :: GenParser Char st Statement
pFor = For <$> (reserved "for" *> optionMaybe iterVar) <*> list <*> pStmBlock
  where list = brackets $ pInt `sepBy` comma    -- TODO point free style -> general purpose function
        iterVar = identifier <* reserved "in"

pTry :: GenParser Char st Statement
pTry = Try <$> (reserved "try" *> pStmBlock)  <*> 
               (reserved "catch" *> pStmBlock)

pProp :: GenParser Char st Statement
pProp = WithProb <$> (reserved "with" *> (reserved "probability" *> float)) <*> (reserved "do" *> pStmBlock) <*> (reserved "otherwise" *> pStmBlock)





