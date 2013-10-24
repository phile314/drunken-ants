module Simplify.Reduce
  ( reduce )
where

import Debug.Trace
import Ast
import qualified Data.Map as M
import Data.Generics.Uniplate.Data

import Control.Monad.State
import Control.Monad.Identity
import Parser

-- | Returns the original or a simplified expression. If the expression is constant,
--   the value of the expression instead of the old expression is returned.
reduce :: Expr -> Expr
reduce (And e1 e2) =
  case (reduce e1) of
    (ConstBool False) -> (ConstBool False)
    (ConstBool True)  -> reduce e2
    e1'               ->
      case reduce e2 of
        (ConstBool False) -> (ConstBool False)
        (ConstBool True)  -> e1'
        e2'               -> (And e1' e2')

reduce (Not e) =
  case (reduce e) of
    (ConstBool b) -> (ConstBool (not b))
    (Not e2)      -> e2
    e             -> (Not e)

reduce (Or e1 e2) =
  case (reduce e1) of
    (ConstBool True)  -> (ConstBool True)
    (ConstBool False) -> reduce e2
    e1'               ->
      case reduce e2 of
        (ConstBool True)  -> (ConstBool True)
        (ConstBool False) -> e1'
        e2'               -> (And e1' e2')
    

reduce ex = ex


