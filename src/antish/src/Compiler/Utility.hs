-- | Provides utility function related to 'Compile' and 'compile'.

{-# LANGUAGE FlexibleContexts #-}

module Compiler.Utility (
    compileAndReorder
  , assemblyLength
  , insertBindings
  ) where

import Ast
import Compiler.Compile
import Compiler.Precompile
import Compiler.Error
import Compiler.TailRecursion
import Control.Monad.State
import Data.Maybe (mapMaybe)

-- | @compileAndReorder inst b1 b2@ compiles a piece of assembly code so that 
--  ______
-- | inst | s0
-- |______|
-- | b1   | s1  = s0 + 1
-- |      |
-- | ...  |
-- |______|
-- | b2   | s2 = s1 + |b1|
-- |      |
-- | ...  | 
-- |______|
-- | b3   | s3 = s2 + |b2|
-- | ...  |
-- |______|
-- @inst@ is executed, based on the instruction the control flow jumps to either @s1@ or @s2@, after one of 
-- this blocks is executed the control flows jump to @s3@
compileAndReorder :: (Jumpable a, Jumpable b) => 
                       (AntState -> AntState -> Instruction) -- ^ The first instruction of the compiled code
                    -> a          -- ^ First assembly code block starting on @s1@
                    -> b          -- ^ Second assembly code block starting on @s2@
                    -> Compile CState [Instruction]
compileAndReorder inst b1 b2 = do
    s0 <- consumeNextState
    l2 <- assemblyLength b2
    i1 <- compileWithJump b1 (+ succ l2)
    i2 <- compileWithJump b2 succ
    let s1 = succ s0
        s2 = s1 + length i1
    return $ [inst s1 s2] ++ i1 ++ i2

-- TODO
-- A more efficient implementation of assemblyLenght should be provided, in addition a class for this 
-- feature should be declared and instance for each compilable element shold be provided

-- | Returns the length of the assembly code generated when compiling a piece of code.
assemblyLength :: Compilable a => a -> Compile CState Int
assemblyLength b = do 
  s <- get
  case runCompile (compile b) s of    -- TODO can we avoid to compile twice?
    Right (i,_) -> return $ length i
    Left e      -> throwError e

-- | Insert each bindining in the scope, taking care of handling mutual recursive declaration
insertBindings :: 
  Compilable StmBlock => [Binding]  -- ^ The bindings being added
                      -> StmBlock   -- ^ The block in which these bindingns are defined
                      -> Compile CState [Instruction]
insertBindings bs b = do
  let recs = mapMaybe isRecursive bs
  newScope >> mapM_ (insertBinding recs) bs
  i <- compile b
  removeScope >> return i 
    where isRecursive (FunDecl Rec iden _ _) = Just iden
          isRecursive _                      = Nothing


-- | Inserts a binding in the proper environment
insertBinding ::
 (Compilable StmBlock) => [Identifier]  -- ^ Recursive definition
                       -> Binding       -- ^ The binding being inserted
                       -> Compile CState ()
insertBinding _ (VarDecl iden expr) = 
  addVarDecl iden expr

insertBinding _ (FunDecl NonRec iden args b) = 
  addFunDecl iden (length args) NonRec c
    where c = precompile b args

insertBinding recs (FunDecl Rec iden [] b) = do
  let c = precompileRecFun iden b
  checkTailRecursive iden b recs
  addFunDecl iden 0 Rec c

insertBinding _ (FunDecl Rec iden xs _) = 
  throwError $ InvalidRecFun iden xs
