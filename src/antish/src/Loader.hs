-- | This module defines an 'Loader', an Error monad that collects
-- both Parser and Compiler errors.

module Loader where

import Control.Monad.Error
import Compiler.Error
import Text.Parsec.Error

data LError = P ParseError  -- Encodes an error during the parsing phase
            | C CError      -- Encodes an error during the compiling phase
            | G String      -- Encodes a generic error

instance Show LError where
  show (P e) = "Parser Error: " ++ show e
  show (C e) = "Compiler Error: " ++ show e
  show (G s) = "General Error: " ++ s 

type Loader = ErrorT LError IO

instance Error LError where
  strMsg = G

