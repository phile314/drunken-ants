module Ants.Parser where

import Ast
import Lexer
import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Data.List

type Parser a = P (Str Char String LineColPos) a


keywords = ["if", "then", "else", "try", "catch", "for", "with", "probability"]

parseProg :: String -> Either Program String
parseProg = parse pProgram

parse :: Parser a -> String -> Either a String
parse p inp = case parse_h ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp) of
    (a, [])   -> Left a
    (_, errs) -> Right $ show_errors errs
    where
        show_errors :: (Show a) => [a] -> String
        show_errors = intercalate "\n" . (map show)


pProgram :: Parser Program
pProgram = undefined

pStmBlock :: Parser StmBlock
pStmBlock = StmBlock <$> ( ((:[]) <$> pStm) <|> pSym '{' *> (pListSep (pLF) pStm) <* pSym '}')

pStm :: Parser Statement
pStm = IfThenElse <$ pKey "if" <*> pExpr <* pKey "then" <*> pStmBlock <* pKey "else" <*> pStmBlock
   <<|> For <$ pKey "for" <*> pReturn Nothing <*> pExpr <*> pStmBlock
   <<|> Call <$> pExpr



pExpr' :: Parser Expr
pExpr' =  (pSym '(' *> pSpaces *> pExpr <* pSpaces <* pSym ')')
  <|> ConstInt <$> pInteger
  <|> VarAccess <$> pIdentifier
  <|> OpCall <$ pSym '[' <*> pExpr <*> pSymbol ".." <*> pExpr <* pSym ']'

pExpr = (OpCall <$> pExpr' <* pSpaces <*> pOp <* pSpaces <*> pExpr)
  <|> Appl <$> pExpr' <* pSpaces <*> pExpr
  <|> pExpr'

pOp = pSymbol "&&" <|> pSymbol "||"

pIdentifier =  (:) <$> pLetter <*> pMany (pLetter <|> pDigit)


pKey :: String -> Parser String
pKey str | str `elem` keywords = pSymbol str
pKey _   | otherwise           = error "Invalid keyword"
pKey' :: String -> Parser String
pKey' str = pSpaces *> pKey str <* pSpaces
