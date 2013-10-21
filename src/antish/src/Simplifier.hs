module Simplify
  ( simplify, shrinkStm, propConsts, inline, ProgTrans (..), test1, test2 )
where

import Debug.Trace
import Ast
import qualified Data.Map as M
import Data.Generics.Uniplate.Data

data Env = Env
  { bindings :: M.Map String BindRHS
  , callStack :: [(String, [Expr], String)] }

type FunInfo = ([String], Statement)

data BindRHS
  = BFun FunInfo
  | BVar Expr

instance Show BindRHS where
  show (BFun (ps, st)) = "BFun " ++ show ps
  show (BVar e)        = "BVar " ++ show e
  

emptyEnv = Env { bindings = M.empty, callStack = [] }

enlEnv :: Env -> [Binding] -> Env
enlEnv (Env bs cs) bsn = (Env (enlEnv' bs bsn) cs)
  where
    enlEnv' env ((VarDecl id ex):bs) = M.insert id (BVar ex) (enlEnv' env bs)
    enlEnv' env ((FunDecl id ps st):bs) = M.insert id (BFun (ps, st)) (enlEnv' env bs)
    enlEnv' env [] = env

putEnv :: Env -> String -> Expr -> Env
putEnv (Env bs cs) id ex = (Env (M.insert id (BVar ex) bs) cs)

enterFun :: Env -> [String] -> [Expr] -> String -> (Env, String)
enterFun (Env bs cs) ps es id = ((Env bs' cs'), (id ++ "TODO labels are not unique!!!"))
  where
    bs' = foldl (\a (p, e) -> M.insert p (BVar e) a) bs $ zip ps es 
    cs' = (id, es, (id ++ "TODO labels are not unique!!!")):cs

lookupVar :: String -> Env -> Expr
lookupVar id (Env bs _) = case (bs M.! id) of
  (BVar v) -> v


lookupFun :: String -> Env -> FunInfo 
lookupFun id (Env bs _) = trace (id ++ "--" ++show bs) $ case (bs M.! id) of
  (BFun f) -> f

isBuiltin :: String -> Bool
isBuiltin id = id `elem` ["move", "turn", "drop", "pickUp"]

type PTName = String

data ProgTrans = ProgTrans 
  { name :: PTName,
    transf :: Program -> Program }


simplify :: Program -> Program
simplify =
      (transf propConsts) . (transf inline)


shrinkStm = ProgTrans
  { name = "Replace statement blocks with one element by their content."
  , transf = transformBi sStm }
  where
    sStm (StmBlock [s]) = s
    sStm s              = s

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
  { name = "Inline variables, functions and unroll loops"
  , transf = descendBiM (sStm emptyEnv) }
  where
    sStm :: Env -> Statement -> Statement
    sStm env (Let bs ss)            =
      let env' = enlEnv env bs
        in (descendBi (sStm env') ss)

    sStm env (IfThenElse ex s1 s2)  = (IfThenElse (inline' env ex) (descendBi (sStm env) s1) (descendBi (sStm env) s2))
    sStm env f@(FunCall id exs)     =
      if isBuiltin id then
        (FunCall id (map (inline' env) exs))
      else
        case isRec env f of
          (Just lbl) -> (JumpTo lbl)
          _ -> let (params, st) = lookupFun id env
                   exs' = map (inline' env) exs
                   (env', lbl) = enterFun env params exs' id
                 in StmBlock [ (Label lbl), (descendBi (sStm env') st)]
 
    sStm env (For Nothing exs st)   = (StmBlock (map (const $ descendBi (sStm env) st) exs))
    sStm env (For (Just id) exs st) =
      let eval = reduce . (inline' env)
          f e = descendBi (sStm (putEnv env id (eval e))) st
        in StmBlock (map f exs)

    sStm env s                      = descend (sStm env) s
    inline' :: Env -> Expr -> Expr
    inline' env = 
      let f (VarAccess id) = lookupVar id env
          f  e             = e
        in transform f

isRec :: Env -> Statement -> Maybe String
isRec (Env _ cs ) f = isRec' cs f
  where
    isRec' ((id2, exs2, lbl):cs) (FunCall id exs) | id2 == id && exs2 == exs = Just lbl
    isRec' (_:cs) f = isRec' cs f
    isRec' [] _ = Nothing

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


test1 = Program (Let [VarDecl "x" (ConstBool True)] (IfThenElse (And (ConstBool True) (VarAccess "x")) (DropCall) (MoveCall)))
test2 = Program (For (Just "x") [(ConstBool True), (ConstBool False)] (IfThenElse (And (ConstBool True) (VarAccess "x")) (DropCall) (MoveCall)))
