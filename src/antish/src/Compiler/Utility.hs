-- | Provides utility function related to 'Compile' and 'compile'.

{-# LANGUAGE FlexibleContexts #-}

module Compiler.Utility (
    safeFunCall
  , unsafeFunCall
  , assemblyLength
  , compileIf
  , compileWithMarker
  , compileAndReorder
  , catchFunNotInScope
  , compileFunCall
  ) where

import Ast
import Compiler.Compile
import Compiler.Precompile
import Compiler.Error
import Compiler.Eval
import Control.Monad.State

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


-- | Compiles an arbitrary IfThenElse statement.
-- The expression involved must be already reduced to basic values.
compileIf :: (Compilable Statement, Jumpable StmBlock) => 
             Expr       -- ^ A reduced Boolean expression
          -> StmBlock   -- ^ The "then" branch
          -> StmBlock   -- ^ The "else" branch
          -> Compile CState [Instruction]
compileIf (And e1 e2) b1 b2 = do
  [Sense c1 s1 _ sd1] <- compile (IfThenElse e1 (StmBlock []) (StmBlock []))
  if2@((Sense _ s3 s4 _):_) <- compileIf e2 b1 b2
  return $ (Sense c1 s1 s4 sd1):if2

compileIf (Or e1 e2) b1 b2 = do
  [Sense c1 s0 _ sd1] <- compileIf e1 (StmBlock []) (StmBlock [])
  if2@((Sense _ s1 s2 _):_) <- compileIf e2 b1 b2
  return $ (Sense c1 s1 s0 sd1):if2

compileIf (Not e) b1 b2 = do
  (Sense c s1 s2 sd):xs <- compileIf e b1 b2
  return $ (Sense c s2 s1 sd):xs

compileIf (Condition c sd) b1 b2 = compileAndReorder ifte b1 b2
  where ifte s1 s2 = Sense sd s1 s2 c


-- | Performs the compilation only if the given 'Expr' is a valid 'MarkerNumber',
-- otherwise it throws a 'CError'.
compileWithMarker :: (MarkerNumber -> Compile CState [Instruction])
                  -> Expr
                  -> Compile CState [Instruction]
compileWithMarker f e = do
  ConstInt n <- eval EInt e
  if validMarkerNumber n 
      then f n
      else throwError $ InvalidMarkerNumber n

-- | Handler for FunNotInScope error.
-- A recursive function might be compiled right now, thus the function is not yet in scope, but 
-- the recursive call has already been tracked. If this is the case a jump back to this point
-- is generated, otherwise the error is propagated.
catchFunNotInScope :: [Expr]  -- The arguments given (must be 0 for a recursive function)
                  -> CError  -- Only 'FunNotInScope' can be recovered
                  -> Compile CState [Instruction]
catchFunNotInScope args (FunNotInScope iden) = do
  s <- getRecursiveCall iden
  case s of 
   Nothing -> throwError (FunNotInScope iden)
   Just s0 -> do checkArgs iden 0 args 
                 generate $ [Flip 1 s0 s0]    -- Jump back to starting point

catchFunNotInScope _ e = throwError e


-- | Compiles a function call, given its definition.
-- A 'CError' is thrown if the function is called with the wrong number of parameters.
compileFunCall :: Identifier  -- The name of the function
               -> [Expr]      -- The arguments passed to the function
               -> (Recursive, Int, [Expr] -> Compile CState [Instruction]) -- The function definition
               -> Compile CState [Instruction]
compileFunCall iden args (Rec, nargs, body) = do
  checkArgs iden nargs args
  s <- getRecursiveCall iden
  case s of 
    Just s0 -> generate $ [Flip 1 s0 s0]    -- Jump back to starting point
    Nothing -> body args

compileFunCall iden args (NonRec, nargs, body) = checkArgs iden nargs args >> body args


-- | @'checkArgs iden nargs args'@, throws an error if the number of arguments expected by the function
-- (@nargs@) does not match the number of arguments given (@args@).
checkArgs :: Identifier -> Int -> [Expr] -> Compile CState ()
checkArgs iden nargs args = do
  let given = length args
      wrongNum = WrongNumberParameters iden given nargs
  if nargs /= given
    then throwError wrongNum 
    else return ()


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


-- | Returns the length of the assembly code generated when compiling a piece of code.
assemblyLength :: Compilable a => a -> Compile CState Int
assemblyLength b = do 
  s <- get
  case runCompile (compile b) s of    -- TODO can we avoid to compile twice?
    Right (i,_) -> return $ length i
    Left e      -> throwError e
