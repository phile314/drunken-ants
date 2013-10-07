module Ants.Parser where

import Ast
import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Data.List

type Parser a = P (Str Char String LineColPos) a


keywords = ["if", "then", "else", "try", "catch", "for", "with", "probability", "let"]

parseProg :: String -> Either String Program
parseProg = parse pProgram

parse :: Parser a -> String -> Either String a
parse p inp = case parse_h ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp) of
    (a, [])   -> Right a
    (_, errs) -> Left $ show_errors errs
    where
        show_errors :: (Show a) => [a] -> String
        show_errors = intercalate "\n" . (map show)


pProgram :: Parser Program
pProgram = Program <$> pStmBlock'

pNewline :: Parser String
pNewline = (pLF *> pure "NEWLINE") <|> (pCR *> pLF *> pure "NEWLINE")
pNewline1 :: Parser String
pNewline1 = (pMany pWSp) *> pNewline

pStmBlock :: Parser StmBlock
pStmBlock = (StmBlock <$> (:[]) <$> pSp pStm) <|> pSym '{' *> pSpaces *> pStmBlock' <* pSpaces <* pSym '}' <* pSpaces

pStmBlock' :: Parser StmBlock
pStmBlock' = StmBlock <$> (pListSep (pSpaces <* pSym ';' <* pSpaces ) (pStm <* pSpaces))

pStm :: Parser Statement
pStm = IfThenElse <$ pKey1 "if" <*> pExpr <* pSpaces <* pKey1 "then" <*> pStmBlock <*> (( pKey1 "else" *> pStmBlock) <|> pure (StmBlock []))
   <<|> For <$ pKey1 "for" <*> pReturn Nothing <*> pExpr <*> pStmBlock
   <<|> Let <$ pKey1 "let" <*> pListSep (pWSp <* pSym ',' <* pSpaces) pBinding
   <<|> Try <$ pKey1 "try" <*> pStmBlock <* pKey1 "with" <*> pStmBlock <* pKey1 "catch" <*> pStmBlock
   <<|> Call <$> pExpr

pBinding :: Parser Binding
pBinding = VarDecl <$> pIdentifier <* pSp (pSym '=') <*> pStmBlock
  <|> FunDecl <$> pIdentifier <* pSpaces <*> pSome (pIdentifier <* pSpaces) <* pSp (pSym '=') <*> pStmBlock


pExpr' :: Parser Expr
pExpr' =  (pSym '(' *> pSpaces *> pExpr <* pSpaces <* pSym ')')
  <|> IntLit <$> pIntegerRaw
  <|> VarAccess <$> pIdentifier
  <|> OpCall <$ pSym '[' <*> pExpr <*> pSymbol ".." <*> pExpr <* pSym ']'
  <|> ListLit <$> listParser pExpr -- pSym '[' <*> pListSep (pSym ',') pExpr <* pSym ']'


pExpr = (OpCall <$> pExpr' <* pSpaces <*> pOp <* pSpaces <*> pExpr)
  <|> Appl <$> pExpr' <* pWSp <*> pExpr
  <|> pExpr'

ops = ["&&", "||", "+", "-", "*", "/", "^"]
pOp = foldr (\s ac -> pSymbol s <|> ac) pFail ops

--pIdentifier :: Parser String
--pIdentifier = ((:) <$> pLetter <*> pMany (pLetter <|> pDigit))
--pIdentifier = pIdentifier' kwTree (error "")

{--pIdentifier' :: [String] -> Parser String
pIdentifier' su = foldr (\(s:ss) a -> (pSym s *> pFail) <|> ((:) <$> pSym s <*> pIdentifier' ss)) pFail su <<|> pIdentifier' []
pIdentifier' [] = pSome (pLetter <|> pDigit)
--}

--pIdentifier' :: KwTree -> String -> Parser String

--pIdentifier' (Root cs)   _  =             (foldr (\x ac -> ("(" ++ pIdentifier' x ("pLetter") ++ ") OR " ++ ac)) "FALSE" cs) ++ ")"
--pIdentifier' (Node c []) pO =  "(EXP " ++ [c] ++ " AND SOME " ++ pId' ++ ") OR_ELSE (" ++ pO ++ " AND MANY " ++ pId' ++ ")"
--pIdentifier' (Node c cs) pO =  "(EXP " ++ [c] ++ " AND ( " ++ (foldr (\x ac -> "(" ++ (pIdentifier' x pId') ++ " OR " ++ ac) "FALSE" cs) ++ ")) OR_ELSE (" ++ pO ++ " MANY " ++ pId' ++ ")"
--pId' = "(LETTER OR DIGIT)"

pIdentifier :: Parser String
pIdentifier = addLength 0 $ do
  str <- ((:) <$> pLetter <*> pMany (pLetter <|> pDigit))
  if str `elem` keywords then
    pFail
  else
    return str

{--pIdentifier' (Root cs)   _  =             foldr (\x ac -> (pIdentifier' x (pLetter) <|> ac)) pFail cs
pIdentifier' (Node c []) pO = ((:) <$> pSym c <*> pSome pId') <<|> ((:) <$> pO <*> (pMany pId'))
pIdentifier' (Node c cs) pO = ((:) <$> pSym c <*> (foldr (\x ac -> (pIdentifier' x pId')     <|> ac) pFail cs)) <<|> ((:) <$> pO <*> (pMany pId'))

pId' :: Parser Char
pId' =  pLetter <|> pDigit
data KwTree = Node Char [KwTree]
            | Root [KwTree] 
  deriving (Show)
kwTree = Root $ mkTree' keywords



mkTree' :: [String] -> [KwTree]
mkTree' kw = map (\(xs@(x:_)) -> mkTree'' (head x) xs) $ groupBy (\(x:_) (y:_) -> x == y) kw

mkTree'' p kw = Node p (mkTree' (filter (not . null) $ map (drop 1) kw))--}

 
pKey :: String -> Parser String
pKey str | str `elem` keywords = pSymbol str
pKey _   | otherwise           = error "Invalid keyword"
pKey1 :: String -> Parser String
pKey1 str = pKey str <* pWSp

pSp f = pSpaces *> f <* pSpaces

pWSp :: Parser String
pWSp = pMunch (`elem` " \t") <?> "Whitespace"
