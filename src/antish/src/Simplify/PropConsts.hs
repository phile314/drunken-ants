module Simplify.PropConsts
  ( propConsts )
where

import Debug.Trace
import Ast
import Data.Generics.Uniplate.Data

import Control.Monad
import Control.Monad.Identity
import Simplify.Reduce
import Simplify.Types
import Parser


-- | Tries to simplify the tree by removing unreachable code.
propConsts = ProgTrans
  { name = "Propagate Constants and remove unreachable code."
  , transf = (return . transformBi sStmBl) :: Program -> Identity Program }
  where
    sStmBl (StmBlock ss) = (StmBlock (concat $ map sStm ss))
    sStm :: Statement -> [Statement]
    sStm (IfThenElse ex (StmBlock s1) (StmBlock s2)) =
      case (reduce ex) of
        (ConstBool True)  -> s1
        (ConstBool False) -> s2
        ex'               -> [IfThenElse ex' (StmBlock s1) (StmBlock s2)]
    sStm s = [s]


