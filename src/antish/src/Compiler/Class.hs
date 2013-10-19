-- | Defines the compiling operations for the elements of the Ast

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Class where

import Assembly (Instruction, AntState) 
import Compiler.Compile
import Ast
import Control.Monad.State hiding (gets)
import Compiler.Error
import Data.Maybe
import Control.Applicative

class Compilable c where
  -- | Returns a monadic computation that performs the compilation
  compile :: c -> Compile CState [Instruction]
  
instance (Compilable a) => Compilable (Maybe a) where
  compile (Just x) = compile x
  compile Nothing = return []

instance (Compilable a) => Compilable [a] where
  compile xs = getJumpTo >>= compileWithJump xs
 
instance Compilable Program where
  compile (Program smb) = compile smb

instance Compilable StmBlock where
  compile (StmBlock xs) = compile xs

instance Compilable Statement where
  compile (IfThenElse expr b1 b2) = do
    case expr of 
      And e1 e2      -> undefined
      Or  e1 e2      -> undefined
      Not e1         -> undefined
      Condition c sd -> do
        i1 <- compile b1
        generate i1
        i2 <- compile b2
        let res = [Sense sd 0 0 c] ++ i1 ++ i2  -- TODO missing correct jump
        generate res
--    _ -> throwError NotBoolean expr -- BoolExpr will be part of Expr soon

  compile (FunCall ident args) = do 
    (expectedArgs , body) <- lookupFun ident
    let nArgs = length args
    if nArgs /= expectedArgs then throwError $ WrongNumberParameters ident nArgs expectedArgs
      else body args
 
  compile (Let bs b) = do
    newScope 
    mapM_ insertBinding bs 
    i <- compile b
    removeScope
    return i 

  compile (For iden xs b) = do
    bx <- forM xs (\x -> do
                 let body = precompile b $ maybeToList iden
                 body [x])
    generate $ concat bx

  compile (WithProb p (StmBlock b1) (StmBlock b2)) | isProbability p = do
    s0 <- nextState
    i2 <- compileWithJump b2 (+1)   -- TODO i2 currentState is wrong
    let l2 = length i2
    i1 <- compileWithJump b1 (+l2)
    let s1 = s0 + 1
        s2 = s0 + length i1
        odds = round $ 1 / p
    generate $ [Flip odds s1 s2] ++ i1 ++ i2
  compile (WithProb p _ _) = throwError $ InvalidProbability p

  compile (Try b1 b2) = do
    i1 <- compile b1
    i2 <- compile b2
    generate $ i1 ++ i2

  compile (MarkCall n) | validMarkerNumber n  = safeFunCall (Mark n)
  compile (MarkCall n) = throwError $ InvalidMarkerNumber n

  compile (UnMarkCall n) | validMarkerNumber n = safeFunCall (Unmark n)
  compile (UnMarkCall n) = throwError $ InvalidMarkerNumber n

  compile DropCall = safeFunCall Drop

  compile MoveCall = unsafeFunCall Move -- TODO before the next Move you should wait 

  compile (TurnCall d) = safeFunCall (Turn d)
-------------------------------------------------------------------------------
-- Precombilable Definition

-- | This class represents something that can be preprocessed but some information has not been provided 
-- yet for it to be fully compiled, like for instance function declaration and variable access.
class PreCompilable c where
  precompile :: c -> [Identifier] -> ([Expr] -> Compile CState [Instruction])

instance PreCompilable StmBlock where
  precompile b argNames = \args -> do 
    insertParameters argNames args
    i <- compile b
    removeScope      -- Parameters Scope
    generate i

compileWithFailure :: StmBlock -> (AntState -> Compile CState [Instruction])
compileWithFailure sb = \failure -> do
    setOnFailure failure
    i <- compile sb
    unsetOnFailure
    return i 

-- | @'compileWithJump' xs j@ compile the 'Compiable' elements contained in @xs@, so that on normal execution
-- they are executed sequentially and after that the control flow jumpt to @j@.
compileWithJump :: Compilable a => 
                   [a]                   -- ^ What is going to be compied
                -> (AntState -> AntState) -- ^ Where to jump after this piece of code
                -> Compile CState [Instruction]
compileWithJump []  _    = return []
compileWithJump [x] j    = setJumpTo j >> compile x
compileWithJump xs after = do
    let (xs', z) = (init xs, last xs)
    cs <- mapM justNext xs'
    cz <- setJumpTo after >> compile z 
    generate $ (concat cs) ++ cz
  where justNext x = do 
          setJumpTo (+1)
          compile x
 
-------------------------------------------------------------------------------
-- Utility functions

-- | Inserts a binding in the proper environment
insertBinding :: Binding -> Compile CState ()
insertBinding (VarDecl iden expr) = addVarDecl iden expr
insertBinding (FunDecl iden args b) = addFunDecl iden (length args) c
  where c = precompile b args

-- | Compiles a safe function call (i.e. it cannot fail). 
safeFunCall :: (AntState -> Instruction) -> Compile CState [Instruction]
safeFunCall f = do
  s <- goNext
  generate [f s]
  
-- | Compiles an unsafe function call. 'onFailure' field from the state is used.
unsafeFunCall :: (AntState ->  -- ^ Where to jump on normal execution
                  AntState ->  -- ^ Where to jump on failure
                  Instruction) -> Compile CState [Instruction]
unsafeFunCall f = do
  normal      <- goNext 
  failure     <- getOnFailure
  generate [f normal failure]

-- | Checks if the 'MarkerNumber' given is valid
validMarkerNumber :: MarkerNumber -> Bool
validMarkerNumber n = 0 <= n && n <= 5

-- | Checks if the number given is a vaild probability.
isProbability :: (Num a, Ord a) => a -> Bool
isProbability p = p > 0 && p <= 1
