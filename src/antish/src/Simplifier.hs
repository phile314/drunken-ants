module Simplify
  ( simplify, runTransf, propConsts, inline, ProgTrans (..), test1, test2 )
where

import Debug.Trace
import Ast
import qualified Data.Map as M
import Data.Generics.Uniplate.Data

type Env = M.Map String Expr

emptyEnv = M.empty

enlEnv :: Env -> [Binding] -> Env
enlEnv env ((VarDecl id ex):bs) = M.insert id ex (enlEnv env bs)
enlEnv env [] = env

putEnv :: Env -> String -> Expr -> Env
putEnv env id ex = M.insert id ex env

type PTName = String

data ProgTrans = ProgTrans 
  { name :: PTName,
    transf :: Program -> Program }

allTransforms :: M.Map String ProgTrans
allTransforms = buildMap []

buildMap pts = foldl (\a pt -> M.insert (name pt) pt a) M.empty pts


simplify :: Program -> Program
simplify =
      (transf propConsts) . (transf inline)


runTransf :: PTName -> Program -> Program
runTransf nm = transf (allTransforms M.! nm)


propConsts = ProgTrans
  { name = "Propagate Constants"
  , transf =  transformBi sStm }
  where
    sStm (IfThenElse ex s1 s2) =
      case (reduce ex) of
        (ConstBool True)  -> s1
        (ConstBool False) -> s2
        ex'               -> (IfThenElse ex' s1 s2)
    sStm s = s


inline = ProgTrans
  { name = "Inline variables"
  , transf = descendBi (sStm emptyEnv) }
  where
    sStm env (Let bs ss)            = descendBi (sStm (enlEnv env bs)) ss
    sStm env (IfThenElse ex s1 s2)  = (IfThenElse (inline' env ex) (descendBi (sStm env) s1) (descendBi (sStm env) s2))
    sStm env (FunCall id exs)       = (FunCall id (map (inline' env) exs))
    sStm env (For Nothing exs st)   = (For Nothing (map (inline' env) exs) (descend (sStm env) st))
    sStm env (For (Just id) exs st) =
      let eval = reduce . (inline' env)
          f e = descendBi (sStm (putEnv env id (eval e))) st
        in StmBlock (map f exs)

    sStm env s                      = descend (sStm env) s
    inline' :: Env -> Expr -> Expr
    inline' env = 
      let f (VarAccess id) = env M.! id
          f  e             = e
        in transform f


-- | Returns the original or a simplified expression. If the expression is constant,
--   the value of the expression instead of the old expression is returned.
reduce :: Expr -> Expr
reduce (And (ConstBool b1) (ConstBool b2)) = ConstBool (b1 && b2)
reduce (Or  (ConstBool b1) (ConstBool b2)) = ConstBool (b1 || b2)
reduce (Not (ConstBool b1)) = ConstBool (not b1)
reduce ex = ex


test1 = Program (Let [VarDecl "x" (ConstBool True)] (IfThenElse (And (ConstBool True) (VarAccess "x")) (DropCall) (MoveCall)))
test2 = Program (For (Just "x") [(ConstBool True), (ConstBool False)] (IfThenElse (And (ConstBool True) (VarAccess "x")) (DropCall) (MoveCall)))
