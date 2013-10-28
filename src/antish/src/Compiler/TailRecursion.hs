-- | This module provides utilities for dealing with tail recursion

module Compiler.TailRecursion (checkTailRecursive) where

import Ast
import Compiler.Compile
import Compiler.Error
import Data.Maybe (isJust)

-- Throws the 'NotTailRecursive' 'CError' if the given function is not tail recursive.
checkTailRecursive :: 
     Identifier    -- ^ The name of the function under investigation
  -> StmBlock      -- ^ The body of the function
  -> [Identifier]  -- ^ Names of functions declared as recursive, but not yet added in the scope
  -> Compile CState ()
checkTailRecursive iden b recs = do
  tailRecursive <- isTailRecursive b recs
  if tailRecursive 
    then return ()
    else throwError $ NotTailRecursive iden b


-- | Checks if the given function is tail recursive, i.e. eventually calls a recursive function.
isTailRecursive :: StmBlock    -- ^ The body of the recursive function
                -> [Identifier] -- ^ Names of functions declared as recursive, but not yet added in the scope
                -> Compile CState Bool
isTailRecursive (StmBlock []) _      = return False
isTailRecursive (StmBlock xs) recs   = isTailRecursive' (last xs) recs


-- | Checks if the given statement represents a tail recursive call.
isTailRecursive' :: Statement -> [Identifier] -> Compile CState Bool
isTailRecursive' (FunCall f _) recs = 
  catchError (lookupFun f >>= isRecursive) recCall
  where recCall (FunNotInScope f) = return $ elem f recs
        isRecursive (r,_,_)       = return $ r == Rec
isTailRecursive' (IfThenElse _ b1 b2) recs = oneIsTailRecursive b1 b2 recs
isTailRecursive' (Try          b1 b2) recs = oneIsTailRecursive b1 b2 recs
isTailRecursive' (WithProb   _ b1 b2) recs = oneIsTailRecursive b1 b2 recs
isTailRecursive' (Let _   b) recs = isTailRecursive b recs
isTailRecursive' (For _ _ b) recs = isTailRecursive b recs
isTailRecursive' _           _    = return False


-- | Checks if at least one of the given 'StmBlock' is tail recursive
oneIsTailRecursive :: StmBlock -> StmBlock -> [Identifier] -> Compile CState Bool
oneIsTailRecursive b1 b2 recs = do
  t1 <- isTailRecursive b1 recs
  t2 <- isTailRecursive b2 recs
  return $ t1 || t2
