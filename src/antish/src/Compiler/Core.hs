-- | Defines the compiling operations for the elements of the Ast

module Compiler.Core (
    module Compiler.Class
  ) where

import Ast
import Assembly (Instruction, AntState) 
import Compiler.Eval
import Compiler.Error
import Compiler.Class
import Compiler.Precompile
import Compiler.Compile
import Compiler.Utility
import Compiler.TailRecursion
import Compiler.If
import Compiler.BuiltIn
import Compiler.Function
import Control.Monad.State
import Data.Maybe

instance Compilable a => Compilable (Maybe a) where
  compile (Just x) = compile x
  compile Nothing = return []

instance (Compilable a) => Compilable [a] where
  compile xs = getJumpTo >>= compileWithJump xs
 
-- The imported bindings are supposed to be already included in the top level declarations
instance Compilable Program where
  compile (Program _ tl) = compile $ Let tl (StmBlock [])

instance Compilable StmBlock where
  compile (StmBlock xs) = compile xs

instance Compilable Statement where
  compile (IfThenElse expr b1 b2) = do
    expr' <- eval EBool expr
    compileIf expr' b1 b2

  compile (FunCall iden args) =
    let simpleFunCall = lookupFun iden >>= compileFunCall iden args
        recCall       = catchFunNotInScope args in
    catchError simpleFunCall recCall

  compile (Let bs b) = insertBindings bs b

  compile (For iden xs b) = do
    let pb x = (precompile b $ maybeToList iden) [x]
    bx <- forM xs pb
    return $ concat bx

  compile (WithProb p b1 b2) | isProbability p = do
    let odds = round $ 1 / p
    compileAndReorder (Flip odds) b1 b2
  compile (WithProb p _ _) = throwError $ InvalidProbability p

  -- Cannot use compileAndReorder due to the failure handling (must be active only within b1)
  compile (Try b1 b2) = do  
    s0 <- nextState
    l1 <- assemblyLength b1
    l2 <- assemblyLength b2
    setOnFailure (s0 + l1)
    i1 <- compileWithJump b1 (+ succ l2)
    unsetOnFailure
    i2 <- compileWithJump b2 succ
    return $ i1 ++ i2

  compile (MarkCall e) = compileWithMarker (safeFunCall . Mark) e

  compile (UnMarkCall e) = compileWithMarker (safeFunCall . Unmark) e

  compile DropCall = safeFunCall Drop

  compile PickUpCall = unsafeFunCall PickUp

  compile MoveCall = unsafeFunCall Move

  compile (TurnCall e) = do
    CDir d <- eval EDir e
    safeFunCall (Turn d)

  compile (Label lbl) = nextState >>= addLabel lbl >> return []
  compile (JumpTo lbl) = do
    sTo <- lookupLabel lbl
    return [(Flip 1 sTo sTo)]   -- TODO should be generate


instance Compilable c => Jumpable [c] where
  compileWithJump []  _    = return []
  compileWithJump [x] j    = setJumpTo j >> compile x
  compileWithJump xs after = do
      let (xs', z) = (init xs, last xs)
      cs <- mapM justNext xs'
      cz <- setJumpTo after >> compile z 
      return $ (concat cs) ++ cz
    where justNext x = do 
            setJumpTo (+1)
            compile x

instance Jumpable StmBlock where
  compileWithJump (StmBlock xs) = compileWithJump xs

instance Jumpable c => Jumpable (Maybe c) where
  compileWithJump (Just c) j = compileWithJump c j
  compileWithJump Nothing  _ = return []
