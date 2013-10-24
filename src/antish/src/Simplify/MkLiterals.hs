module Simplify.MkLiterals
  ( mkLiterals )
where

import Debug.Trace
import Ast
import Data.Generics.Uniplate.Data

import Control.Monad
import Control.Monad.Identity
import Simplify.Reduce
import Simplify.Types
import Parser

-- | Converts VarAccess nodes to Const... Nodes if possible, for example True and False.
mkLiterals = ProgTrans
  { name = "Converts VarAccess nodes to Const... Nodes if possible, for example True and False."
  , transf = (return . transformBi sExpr) :: Program -> Identity Program }
  where
    sExpr (VarAccess "True") = ConstBool True
    sExpr (VarAccess "False") = ConstBool False
    sExpr e = e
