module Parser.Program where

import Ast
import Control.Applicative hiding ((<|>), many)
import Control.Monad.Trans
import Control.Monad
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Combinator
import Parser.Block
import Parser.LangDef
import Data.Either
import Control.Monad.Error

-- pProgram :: GenParser Char st Program  -- Original
pProgram = Program <$> pImports <*> pTop

-- | Parses top level declarations (no let)
pTop :: GenParser Char st [Binding]
pTop = many pBinding

pImports :: GenParser Char st [String]
pImports = many (reserved "import" *> pModuleName)

pModuleName :: GenParser Char st String
pModuleName = many1 letter
