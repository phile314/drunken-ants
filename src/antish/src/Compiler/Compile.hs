-- | This module defines the 'Compile' monad that represents the compilation of an element of the language
module Compiler.Compile (
      Compile 
    , module Compiler.CompileT
    , Compiler.Compile.empty
    , generate
    , CState
    , lookupFun
    , newScope
    , removeScope
    , insertParameters
    , addVarDecl
    , addFunDecl
    , nextState
    , consumeNextState
    , setJumpTo
    , getJumpTo
    , getOnFailure
    , setOnFailure
    , unsetOnFailure
    , runCompile
    , goNext
  ) where

import Control.Monad.State
import Control.Monad.Identity
import Compiler.CompileT
import Compiler.Error
import Compiler.Scope as Scope
import Ast
import Assembly (Instruction, AntState)

type Compile s a = CompileT s Identity a

-- | Runs the 'Compile' monad
runCompile :: Compile s a -> s -> Either CError (a ,s)
runCompile c s = runIdentity $ runCompileT c s

-------------------------------------------------------------------------------
-- CState definition
-- | The state used in the Compile monad
data CState = CState {   currentState :: AntState -- ^ The first available state
                       , functions :: Scope Identifier FunDef -- ^ The functions scope environment
                       , variables :: Scope Identifier VarDef -- ^ The variables scope environment
                       , jumpTo    :: (AntState -> AntState)  -- ^ Returns where to jump after the current instruction have been executed if it succeds
                       , onFailure :: [AntState]        -- ^ Where to jump on failure
                     } 

-- | 'FunDef' represents a function definition. The first element is the number of parameters and the second
-- is a function that given the exact number of parameters of the correct type returns the assembly code
-- for the function.
type FunDef = (Int, [Expr] -> Compile CState [Instruction])
type VarDef = Expr

-- | The empty CState
empty :: CState
empty = CState 0 Scope.empty Scope.empty (+1) [0]

-------------------------------------------------------------------------------
-- Utility functions

-- | Returns assembly instructions taking care of updating the state correctly.
-- For instance 'currentState' is generated, so that it points to the next available state
-- after producing some assembly code
generate :: [Instruction] -> Compile CState [Instruction]
generate xs = do
  (s,cs) <- get >>= \s -> return (s, currentState s) 
  put (s { currentState = cs + length xs} )
  return xs

-- | Returns the current variable environment
varEnv :: Compile CState (Scope Identifier VarDef)
varEnv = do
  s <- get
  return $ variables s

-- | Returns the current function environment
funEnv :: Compile CState (Scope Identifier FunDef)
funEnv = do
  s <- get
  return $ functions s

-- | Stores in the current scope a variable declaration
addVarDecl ::    Identifier -- ^ The name of the variable declared
              -> Expr       -- ^ The corresponding expression
              -> Compile CState ()
addVarDecl iden expr = do
  env <- varEnv
  modify $ \s -> s { variables = Scope.insert iden expr env} 

-- | Stores in the current scope a function declaration
addFunDecl ::    Identifier -- ^ The name of the function declared
              -> Int        -- ^ The number of parameters
              -> ([Expr] -> Compile CState [Instruction]) -- The precompiled code of the function
              -> Compile CState ()
addFunDecl iden n c = do
  env <- funEnv
  modify $ \s -> s { functions = Scope.insert iden (n, c) env}

-- | Creates a scope in the correct position where the parameters of a function should be looked up
insertParameters :: [Identifier] -> [Expr] -> Compile CState ()
insertParameters args values = do
  let newScope = foldl (flip $ uncurry Scope.insert) Scope.empty $ zip args values
  env <- varEnv
  let newEnv = father newScope env
  modify $ \s -> s { variables = newEnv }

-- | The identifier is looked up among the declared functions.
-- If the function is not in scope the monad throwErrors.
lookupFun :: Identifier -> Compile CState FunDef
lookupFun iden = do 
  env <- funEnv
  case Scope.lookup iden env of
    Just def -> return def 
    Nothing -> throwError $ FunNotInScope iden 

-- | Add a new empty scope both for function and variables as the current scope
newScope :: Compile CState ()
newScope = do
  varScope <- varEnv
  funScope <- funEnv
  modify $ \s -> s { variables = children Scope.empty varScope, functions = children Scope.empty funScope}

-- | Remove the current scope both for function and variables
removeScope :: Compile CState ()
removeScope = do
  varScope <- varEnv
  funScope <- funEnv
  modify $ \s -> s { variables = remove varScope, functions = remove funScope}

-- | Returns the first available 'AntState'
nextState :: Compile CState AntState
nextState = get >>= (return . currentState)

-- | Returns the first available 'AntState' and updates the current 'AntState' with a new available state
consumeNextState :: Compile CState AntState
consumeNextState = do
  s <- get
  let ns = currentState s
  put $ s { currentState = ns + 1}
  return ns

-- | Returns where to jump in case of failure
getOnFailure :: Compile CState AntState
getOnFailure = get >>= (return . head . onFailure)

-- | Returns where to jump next on normal execution based.
getJumpTo :: Compile CState (AntState -> AntState)
getJumpTo = get >>= return . jumpTo

-- | Returns the state where to jump on normal execution based on the current state
goNext  :: Compile CState AntState
goNext = do
  s <- get 
  return $ (jumpTo s) (currentState s)

-- | Sets the 'onJump' function that returns the 'AntState' where to jump on normal execution
setJumpTo :: (AntState -> AntState) -> Compile CState ()
setJumpTo jn = modify $ \s -> s { jumpTo = jn }

-- | Sets locally the 'AntState' where to jump in case of failure
setOnFailure :: AntState -> Compile CState ()
setOnFailure as = modify $ \s -> s {onFailure = as:(onFailure s)}

-- | Unsets the local defined 'AntState' used for jumping on failures.
unsetOnFailure :: Compile CState ()
unsetOnFailure = modify $ \s -> s { onFailure = tail (onFailure s) }
