-- | Defines the compiling operations for the elements of the Ast

module Compiler.Core (
    module Compiler.Class
  ) where

-- TODO
-- compileWithJump should be generalized to something that can be used also by the Try instance of compile
-- A more efficient implementation of assemblyLenght should be provided, in addition a class for this 
-- feature should be declared and instance for each compilable element shold be provided

import Compiler.Eval
import Data.Maybe
import Compiler.Error
import Compiler.Class
import Assembly (Instruction, AntState) 
import Compiler.Compile
import Ast
import Control.Monad.State

instance Compilable a => Compilable (Maybe a) where
  compile (Just x) = compile x
  compile Nothing = return []

instance Compilable a => Compilable [a] where
  compile xs = getJumpTo >>= compileWithJump xs
 
instance Compilable Program where
  compile (Program smb) = compile smb

instance Compilable Statement where
  compile (IfThenElse expr b1 b2) = eval EBool expr >>= compileIf
    where compileIf (And e1 e2)      = undefined
          compileIf (Or  e1 e2)      = undefined
          compileIf (Not e1)         = undefined
          compileIf (Condition c sd) = compileAndReorder ifte b1 b2
            where ifte s1 s2 = Sense sd s1 s2 c 

  compile (FunCall ident args) = do 
    (expectedArgs , body) <- lookupFun ident
    let nArgs = length args
    if nArgs /= expectedArgs
      then throwError $ WrongNumberParameters ident nArgs expectedArgs
      else body args
 
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

  compile (StmBlock xs) = compile xs

  compile (MarkCall e) = compileWithMarker (safeFunCall . Mark) e

  compile (UnMarkCall e) = compileWithMarker (safeFunCall . Unmark) e

  compile DropCall = safeFunCall Drop

  compile PickUpCall = unsafeFunCall PickUp

  compile MoveCall = unsafeFunCall Move -- TODO before the next Move you should wait 

  compile (TurnCall d) = safeFunCall (Turn d)   -- TODO replace with expr

instance PreCompilable Statement where
  precompile b argNames = \args -> do 
    insertParameters argNames args
    i <- compile b
    removeScope      -- Parameters Scope
    return i

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

instance Jumpable Statement where
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

-- | Performs the compilation only if the given 'Expr' is a valid 'MarkerNumber', otherwise it throws 
-- a 'CError'
compileWithMarker :: (MarkerNumber -> Compile CState [Instruction])
                  -> Expr
                  -> Compile CState [Instruction]
compileWithMarker f e = do
     ConstInt n <- eval EInt e
     if validMarkerNumber n then f n
        else throwError $ InvalidMarkerNumber n
