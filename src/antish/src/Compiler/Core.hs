-- | Defines the compiling operations for the elements of the Ast

{-# LANGUAGE FlexibleInstances #-}

module Compiler.Core (
    module Compiler.Class
  ) where

-- TODO
-- A more efficient implementation of assemblyLenght should be provided, in addition a class for this 
-- feature should be declared and instance for each compilable element shold be provided

import Ast
import Assembly (Instruction, AntState) 
import Compiler.Eval
import Compiler.Error
import Compiler.Class
import Compiler.Precompile
import Compiler.Compile
import Compiler.Utility
import Control.Monad.State
import Data.Maybe

instance Compilable a => Compilable (Maybe a) where
  compile (Just x) = compile x
  compile Nothing = return []

instance Compilable a => Compilable [a] where
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

  compile (FunCall ident args) = do 
    (rec, expectedArgs , body) <- lookupFun ident
    let nArgs = length args
        wrongNum = throwError $ WrongNumberParameters ident nArgs expectedArgs
    if nArgs /= expectedArgs 
      then wrongNum
      else compileFunCall rec ident body args
 
  compile (Let bs b) = do
    newScope >> mapM_ insertBinding bs 
    i <- compile b
    removeScope >> return i 

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
    return [(Flip 1 sTo sTo)]

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

-------------------------------------------------------------------------------
-- | Inserts a binding in the proper environment
insertBinding :: Binding -> Compile CState ()
insertBinding (VarDecl iden expr) = addVarDecl iden expr
insertBinding (FunDecl NonRec iden args b) = addFunDecl iden (length args) NonRec c
  where c = precompile b args
insertBinding (FunDecl Rec iden [] b) = do
  let c = precompileRecFun iden b
  tailRecursive <- isTailRecursive b
  if tailRecursive 
    then addFunDecl iden 0 Rec c
    else throwError $ NotTailRecursive iden b
insertBinding (FunDecl Rec iden xs _) = throwError $ InvalidRecFun iden xs
