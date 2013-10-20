-- | Defines the compiling operations for the elements of the Ast

-- TODO
-- compileWithJump should be generalized to something that can be used also by the Try instance of compile
-- A more efficient implementation of assemblyLenght should be provided, in addition a class for this 
-- feature should be declared and instance for each compilable element shold be provided

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Class where

import Assembly (Instruction, AntState) 
import Compiler.Compile
import Ast
import Control.Monad.State hiding (gets)
import Compiler.Error
import Data.Maybe

class Compilable c where
  -- | Returns a monadic computation that performs the compilation
  compile :: c -> Compile CState [Instruction]
  
instance Compilable a => Compilable (Maybe a) where
  compile (Just x) = compile x
  compile Nothing = return []

instance Compilable a => Compilable [a] where
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
      Condition c sd -> let ifte s1 s2 = Sense sd s1 s2 c in
              compileAndReorder ifte b1 b2
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
    i1 <- compileWithJump b1 (+l2)
    unsetOnFailure
    i2 <- compileWithJump b2 (+1)
    return $ i1 ++ i2

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
    return i

-------------------------------------------------------------------------------
-- Jumpable Definition

-- | Represents some code that requires some jump in the control flow
class Compilable c => Jumpable c where
  
  -- | @'compileWithJump' xs j@ compiles the 'Compiable' elements contained in @xs@, so that on normal execution
  -- they are executed sequentially and after that the control flow jumpt to @j@.
  compileWithJump :: c                          -- ^ What is going to be compied
                  -> (AntState -> AntState)     -- ^ Where to jump after this piece of code
                  -> Compile CState [Instruction]

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

instance Jumpable c => Jumpable (Maybe c) where
  compileWithJump (Just c) j = compileWithJump c j
  compileWithJump Nothing  _ = return []

instance Jumpable StmBlock where
  compileWithJump (StmBlock xs) = compileWithJump xs

-------------------------------------------------------------------------------
-- | @compileAndReorder inst b1 b2@ compiles a piece of assembly code so that 
--
-- [ inst ] s0
--
-- [ b1   ] s1  = s0 + 1
--
-- [ ...  ]
--
-- [ b2   ] s2 = s1 + |b1|
--
-- [ ...  ] 
--
-- [ b3   ] s3 = s2 + |b2|
--
-- inst is executed, based on the instruction the control flow jumps to either @s1@ or @s2@, after one of 
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

-- | Returns the length of the assembly code generated when compiling a 'StmBlock'.
assemblyLength :: Compilable a => a -> Compile CState Int
assemblyLength b = do 
  s <- get
  case runCompile (compile b) s of    -- TODO can we avoid to compile twice?
    Right (i,_) -> return $ length i
    Left e      -> throwError e

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
unsafeFunCall :: (   AntState      -- ^ Where to jump on normal execution
                  -> AntState      -- ^ Where to jump on failure
                  -> Instruction)
                  -> Compile CState [Instruction]
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
