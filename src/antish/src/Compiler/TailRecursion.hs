-- | This module provides utilities for dealing with tail recursion

module Compiler.TailRecursion (checkTailRecursive) where

import Ast
import Compiler.Compile
import Compiler.Error
import Data.Maybe (isJust)

checkTailRecursive :: Identifier -> StmBlock -> Compile CState ()
checkTailRecursive iden b = do
  tailRecursive <- isTailRecursive b
  if tailRecursive 
    then return ()
    else throwError $ NotTailRecursive iden b

-- | Checks if the given function is tail recursive, i.e. eventually calls a recursive function.
isTailRecursive :: StmBlock    -- ^ The body of the recursive function
                -> Compile CState Bool
isTailRecursive (StmBlock []) = return False
isTailRecursive (StmBlock xs) = isTailRecursive' (last xs)


-- | Checks if the given statement represents a tail recursive call.
isTailRecursive' (FunCall f _) = 
  catchError (lookupFun f >>= isRecursive) recCall
  where recCall (FunNotInScope f) = getRecursiveCall f >>= return . isJust
        isRecursive (r,_,_) = return $ r == Rec
isTailRecursive' (IfThenElse _ b1 b2) = bothTailRecursive b1 b2
isTailRecursive' (Try          b1 b2) = bothTailRecursive b1 b2
isTailRecursive' (WithProb   _ b1 b2) = bothTailRecursive b1 b2
isTailRecursive' (Let _   b) = isTailRecursive b
isTailRecursive' (For _ _ b) = isTailRecursive b
isTailRecursive' _ = return False


-- | Checks if both the given 'StmBlock' are tail recursive
bothTailRecursive :: StmBlock -> StmBlock -> Compile CState Bool
bothTailRecursive b1 b2 = do
  t1 <- isTailRecursive b1
  t2 <- isTailRecursive b2
  return $ t1 && t2
