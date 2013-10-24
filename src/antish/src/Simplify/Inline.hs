{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Simplify.Inline
  ( inline, progToLet, maxDepth )
where


import Debug.Trace
import Simplify.Reduce
import Simplify.Types
import Simplify.InlineT

import qualified Data.Map as M
import Data.Generics.Uniplate.Data

import qualified Control.Monad.Error as E
import Control.Monad.State
import Control.Monad.Identity
import Parser

-- | The maximal allowed recursion depth. This only applies to recursive calls which are not tail-calls.
maxDepth = 20

type Bindings = M.Map String BindRHS

data StateCS a = S (StateT Int Identity a)

data Env = Env
  { bindings :: Bindings
    --             func-name, params, def-lbl
  , callStack :: [(String, [Expr], String)]
  , callDepth :: Int }


data BindRHS
  = BFun [String] StmBlock Bindings
  | BVar Expr Bindings

instance Show BindRHS where
  show (BFun ps st env) = "BFun " ++ show ps
  show (BVar e env)        = "BVar " ++ show e
  

emptyEnv = Env { bindings = M.empty, callStack = [], callDepth = 0}

enlEnv :: Env -> [Binding] -> Env
enlEnv (Env bs cs cd) bsn =
  let bs' = f bsn bs'
      bsa = (bs' `M.union` bs)
      f :: [Binding] -> Bindings -> Bindings
      f ((VarDecl id ex):bs)    bs' = M.insert id (BVar ex bsa) $ f bs bs'
      f ((FunDecl id ps st):bs) bs' = M.insert id (BFun ps st bsa) $ f bs bs'
      f []                      bs' = M.empty
    in (Env bs' cs cd)

putEnv :: Env -> String -> Expr -> Env
putEnv (Env bs cs cd) id ex = (Env (M.insert id (BVar ex bs) bs) cs cd)


-- | Updates the environment and the state when entering a function.
enterFun :: Env -> String -> [Expr] -> Inline (Env, String, StmBlock)
enterFun (Env bs cs cd) id es = do
  if cd > maxDepth then
    throwError $ RecursionLimitReached maxDepth
  else do
    cn <- get
    put (cn + 1)
    let lbl = (id ++ (show cn))
    let cs' = (id, (map (normalize . (inlineV bs)) es), lbl):cs

    let (BFun ps st bs') = lookupSym bs id
    let bs'' = foldl (\a (x, y) -> M.insert x (BVar y bs) a) bs' $ zip ps es

    return ((Env bs'' cs' (cd + 1)), lbl, st)


lookupSym :: Bindings -> String -> BindRHS
lookupSym = (M.!)
lookupSym' :: Env -> String -> BindRHS
lookupSym' env id = (bindings env) M.! id

isBuiltin :: String -> Bool
isBuiltin id = id `elem` ["Move", "Turn", "Drop", "PickUp"]

-- | Wraps all bindings in a Let. This conversion makes sure that there is only the
--   internal __main__ binding on top, which allows us to skip the top level
--   bindings when building the environment.
progToLet (Program is bs) = (Program is [FunDecl "__main__" [] (StmBlock [(Let bs (StmBlock [FunCall "main" []]))])])

-- | Inlines variables, functions and unrolls loops. The returned tree
--   is garantued to have no function calls (apart from builtin functions)
--   and to contain no variables. Tail-calls are converted to iterative
--   code, non tail-call recursion is limited to a depth of `maxDepth`.
inline = ProgTrans
  { name = "Inline variables, functions and unroll loops."
  , transf = transf' }
  where
    transf' :: Program -> Either TError Program
    transf' p = do
      let s = descendBiM (sStmBl True emptyEnv) (progToLet p) :: Inline Program
      let k = runIdentity $ runInlineT s 0
      case k of
        (Left e) -> E.throwError e
        (Right (p, _)) -> return p

    sStmBl :: Bool -> Env -> StmBlock -> Inline StmBlock
    sStmBl _ _ (StmBlock []) = return (StmBlock [])
    sStmBl mt env (StmBlock ss) = do
      ss' <- mapM2 (sStm False env) (sStm mt env) ss
      return (StmBlock $ concat ss')

    sStmBl' :: Bool -> Env -> StmBlock -> Inline [Statement]
    sStmBl' mt env sb = do
      (StmBlock ss) <- sStmBl mt env sb
      return ss

    sStm :: Bool -> Env -> Statement -> Inline [Statement]
    sStm mt env (Let bs ss)            =
      let env' = enlEnv env bs
        in sStmBl' mt env' ss

    sStm mt env (IfThenElse ex s1 s2)  = do
      s1' <- sStmBl mt env s1
      s2' <- sStmBl mt env s2
      return [IfThenElse (inlineV' env ex) s1' s2']

    sStm mt env f@(FunCall id exs)     =
      if isBuiltin id then
        return [FunCall id (map (inlineV' env) exs)]
      else do
        l <- isRec env f
        case (l, mt) of
          ((Just lbl), True) -> return [JumpTo lbl]
          _ -> do
                 (env', lbl, st) <- enterFun env id exs
                 s' <- sStmBl' mt env' st
                 return $ (Label lbl):s'

    sStm mt env (For Nothing exs st)   = do
      st' <- mapM2 (const $ sStmBl' False env st) (const $ sStmBl' mt env st) exs
      return $ concat st'
    sStm mt env (For (Just id) exs st) =
      let eval = reduce . (inlineV' env)
          f m e = sStmBl' m (putEnv env id (eval e)) st
        in mapM2 (f False) (f mt) exs >>= (return . concat)


    sStm mt env s                      = do
      r <- descendBiM (sStmBl mt env) s
      return [r]

mapM2 f1 f2 (l1:(ls@(_:_))) = do
                            s <- f1 l1
                            ss <- mapM2 f1 f2 ls
                            return (s:ss)
mapM2 f1 f2 [l]        = do
                            s <- f2 l
                            return [s]
mapM2 _ _ []           = return []


-- | Checks whether the function to be called in the given function call has already
--   been called before with the same arguments. If so, it returns the label located
--   before the function code.
isRec :: Env -> Statement -> Inline (Maybe String)
isRec env f = isRec' (callStack env) f
  where
    isRec' ((id2, exs2, lbl):cs) (FunCall id exs) | id == id2 && check exs exs2 = return $ Just lbl
    isRec' (_:cs) f = isRec' cs f
    isRec' [] _     = return Nothing
    check exs1 exs2 = all (\(a,b) -> (normalize $ inlineV' env a) == b) $ zip exs1 exs2


inlineV :: Bindings -> Expr -> Expr
inlineV bs = 
  let f (VarAccess id) = let (BVar e bs') = lookupSym bs id in inlineV bs' e
      f  e             = e
    in transform f
inlineV' env e = inlineV (bindings env) e


normalize :: Expr -> Expr
normalize = reduce

