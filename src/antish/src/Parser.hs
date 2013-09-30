module Parser where

import Control.Applicative
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
parens = P.parens lexer
symbol = P.symbol lexer
