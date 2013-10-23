module Simplify
  ( simplify, propConsts, inline, ProgTrans (..), test1, test2 )
where

import Debug.Trace
import Ast
import qualified Data.Map as M
import Data.Generics.Uniplate.Data

import Control.Monad.State
import Control.Monad.Identity
import Parser

data Env = Env
  { bindings :: M.Map String BindRHS
    --             func-name, params, def-lbl
  , callStack :: [(String, [Expr], String)]}

type FunInfo = ([String], StmBlock)

data BindRHS
  = BFun FunInfo
  | BVar Expr

instance Show BindRHS where
  show (BFun (ps, st)) = "BFun " ++ show ps
  show (BVar e)        = "BVar " ++ show e
  

emptyEnv = Env { bindings = M.empty, callStack = []}

enlEnv :: Env -> [Binding] -> Env
enlEnv (Env bs cs) bsn = (Env (enlEnv' bs bsn) cs)
  where
    enlEnv' env ((VarDecl id ex):bs) = M.insert id (BVar ex) (enlEnv' env bs)
    enlEnv' env ((FunDecl id ps st):bs) = M.insert id (BFun (ps, st)) (enlEnv' env bs)
    enlEnv' env [] = env

putEnv :: Env -> String -> Expr -> Env
putEnv (Env bs cs) id ex = (Env (M.insert id (BVar ex) bs) cs)


-- | Updates the environment and the state when entering a function.
enterFun :: Env -> [String] -> [Expr] -> String -> StateCS (Env, String)
enterFun (Env bs cs) ps es id = do
  cn <- get
  put (cn + 1)
  let lbl = (id ++ (show cn))
  let cs' = (id, es, lbl):cs
  return ((Env bs' cs'), lbl)
  where
    bs' = foldl (\a (p, e) -> M.insert p (BVar e) a) bs $ zip ps es 

-- | Returns the value of the variable with the given name
--   or throws an error if no such function exists.
lookupVar :: String -> Env -> Expr
lookupVar id (Env bs _) = case (bs M.! id) of
  (BVar v) -> v

-- | Returns the function with the given name
--   or throws an error if no such function exists.
lookupFun :: String -> Env -> FunInfo 
lookupFun id (Env bs _) = case (bs M.! id) of
  (BFun f) -> f

isBuiltin :: String -> Bool
isBuiltin id = id `elem` ["move", "turn", "drop", "pickUp"]

-- | A program transformation.
data ProgTrans m = ProgTrans 
  { name :: String,
    transf :: Program -> m Program }

-- | Produces a simpler version of a program. The result
--   is semantically equivalent to the original version.
simplify :: Program -> Program
simplify p =
      runIdentity $ ((transf propConsts) p) >>= (transf $ inline)


-- | Resolves imports.
resImports :: Program -> Loader Program
resImports = undefined


-- | Tries to simplify the tree by removing unreachable code.
propConsts = ProgTrans
  { name = "Propagate Constants"
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


type StateCS a = State Int a

-- | Inlines variables, functions and unrolls loops. The returned tree
--   is garantued to have no function calls (apart from builtin functions)
--   and to contain no variables.
inline = ProgTrans
  { name = "Inline variables, functions and unroll loops"
  , transf = \p -> (return . fst $ runState (descendBiM (sStmBl True emptyEnv) p) 0) :: Identity Program}
  where
    sStmBl :: Bool -> Env -> StmBlock -> StateCS StmBlock
    sStmBl _ _ (StmBlock []) = return (StmBlock [])
    sStmBl mt env (StmBlock ss) = do
      ss' <- mapM (sStm False env) (take ((length ss) - 1) ss)
      se' <- sStm mt env (head $ drop ((length ss) - 1) ss)
      return (StmBlock $ (concat ss') ++ se')

    sStmBl' mt env sb = do
      (StmBlock ss) <- sStmBl mt env sb
      return ss

    sStm :: Bool -> Env -> Statement -> StateCS [Statement]
    sStm mt env (Let bs ss)            =
      let env' = enlEnv env bs
        in sStmBl' mt env' ss

    sStm mt env (IfThenElse ex s1 s2)  = do
      s1' <- sStmBl mt env s1
      s2' <- sStmBl mt env s2
      return [IfThenElse (inline' env ex) s1' s2']

    sStm mt env f@(FunCall id exs)     =
      if isBuiltin id then
        return [FunCall id (map (inline' env) exs)]
      else do
        l <- isRec env f
        case (l, mt) of
          ((Just lbl), True) -> return [JumpTo lbl]
          _ -> do
                 let (params, st) = lookupFun id env
                 let exs' = map (inline' env) exs
                 (env', lbl) <- enterFun env params exs' id
                 s' <- sStmBl' mt env' st
                 return $ (Label lbl):s'

    sStm mt env (For Nothing exs st)   = do
      -- TODO not good enough
      st' <- mapM (const $ sStmBl' False env st) exs
      return $ concat st'
    sStm mt env (For (Just id) exs st) =
      -- TODO
      let eval = reduce . (inline' env)
          f e = sStmBl' False (putEnv env id (eval e)) st
        in mapM f exs >>= (return . concat)


    sStm mt env s                      = do
      r <- descendBiM (sStmBl mt env) s
      return [r]

    inline' :: Env -> Expr -> Expr
    inline' env = 
      let f (VarAccess id) = lookupVar id env
          f  e             = e
        in transform f

-- | Checks whether the function to be called in the given function call has already
--   been called before with the same arguments. If so, it returns the label located
--   before the function code.
isRec :: Env -> Statement -> StateCS (Maybe String)
isRec env f = isRec' (callStack env) f
  where
    isRec' ((id2, exs2, lbl):cs) (FunCall id exs) | id2 == id && exs2 == exs = return $ Just lbl
    isRec' (_:cs) f = isRec' cs f
    isRec' [] _ = return Nothing

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


test1 = Program [] $ [VarDecl "x" (ConstBool True), FunDecl "main" [] (StmBlock [IfThenElse (And (ConstBool True) (VarAccess "x")) (StmBlock [DropCall]) (StmBlock [MoveCall])])]
test2 = Program [] $ [FunDecl "main" [] (StmBlock [For (Just "x") [(ConstBool True), (ConstBool False)] (StmBlock [IfThenElse (And (ConstBool True) (VarAccess "x")) (StmBlock [DropCall]) (StmBlock [MoveCall])])])]
