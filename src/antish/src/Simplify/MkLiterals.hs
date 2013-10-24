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


mkLiterals = ProgTrans
  { name = "TODO"
  , transf = (return . transformBi sExpr) :: Program -> Identity Program }
  where
--    sStmBl (StmBlock ss) = (StmBlock (concat $ map sStm ss))
{-    sStm :: Statement -> [Statement]
    sStm (IfThenElse ex (StmBlock s1) (StmBlock s2)) =
      case (reduce ex) of
        (ConstBool True)  -> s1
        (ConstBool False) -> s2
        ex'               -> [IfThenElse ex' (StmBlock s1) (StmBlock s2)]
    sStm s = [s]
-}
    sExpr (VarAccess "True") = ConstBool True
    sExpr (VarAccess "False") = ConstBool False
    sExpr e = e
