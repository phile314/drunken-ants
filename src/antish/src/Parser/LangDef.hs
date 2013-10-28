module Parser.LangDef
(
    module Text.Parsec.Combinator
  , module Text.Parsec.String
  , module Parser.LangDef
  ) where


import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Ast

antish :: P.LanguageDef st
antish = haskellDef

lexer = P.makeTokenParser antish

-- The library functions tailored on our language definition
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
natural = P.natural lexer
float = P.float lexer
parens = P.parens lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
comma = P.comma lexer
braces = P.braces lexer
brackets = P.brackets lexer
semi = P.semi lexer
whites = P.whiteSpace lexer

