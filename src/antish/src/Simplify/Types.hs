module Simplify.Types
  ( ProgTrans (..)
  , module Ast )
where

import Debug.Trace
import Ast
import qualified Data.Map as M
import Data.Generics.Uniplate.Data

import Control.Monad.State
import Control.Monad.Identity
import Parser


-- | A program transformation.
data ProgTrans m = ProgTrans 
  { name :: String,
    transf :: Program -> m Program }

