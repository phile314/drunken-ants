-- | This module defines the 'Compile' monad that represents the compilation of an element of the language
module Compiler.Compile (
      Compile 
    , module Compiler.CompileT
    , Compiler.Compile.empty
    , generate
    , CState
    , lookupFun
    , lookupVar
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
    , setRecursiveCall
    , unsetRecursiveCall
    , getRecursiveCall
    , addLabel
    , lookupLabel
  ) where

import Control.Monad.State hiding (gets)
import Control.Monad.Identity
import Compiler.CompileT
import Compiler.Error
import Compiler.Scope as Scope
import Ast
import Assembly (Instruction, AntState)
import qualified Data.Map as M

type Compile s a = CompileT s Identity a

-- | Runs the 'Compile' monad
runCompile :: Compile s a -> s -> Either CError (a ,s)
runCompile c s = runIdentity $ runCompileT c s

gets :: (s -> a) -> Compile s a
gets f = get >>= return . f

-------------------------------------------------------------------------------
-- CState definition
-- | The state used in the Compile monad
data CState = CState {   currentState :: AntState -- ^ The first available state
                       , functions :: Scope Identifier FunDef -- ^ The functions scope environment
                       , variables :: Scope Identifier VarDef -- ^ The variables scope environment
                       , jumpTo    :: (AntState -> AntState)  -- ^ Returns where to jump after the current instruction have been executed if it succeds
                       , onFailure :: [AntState]        -- ^ Where to jump on failure
                       , recursive :: M.Map Identifier AntState   -- ^ Tracks the tail recursive 
                       , labels    :: M.Map String AntState
                     } 

-- | 'FunDef' represents a function definition. The first element is the number of parameters and the second
-- is a function that given the exact number of parameters of the correct type returns the assembly code
-- for the function.
type FunDef = (Recursive, Int, [Expr] -> Compile CState [Instruction])
type VarDef = Expr

-- | The empty CState
empty :: CState
empty = CState 0 Scope.empty Scope.empty (+1) [0] M.empty M.empty

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
varEnv = gets variables

-- | Returns the current function environment
funEnv :: Compile CState (Scope Identifier FunDef)
funEnv = gets functions

-- | Returns the recursive environment (recursive calls' antstates)
recEnv :: Compile CState (M.Map Identifier AntState)
recEnv = gets recursive

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
              -> Recursive  -- ^ Is the function recursive
              -> ([Expr] -> Compile CState [Instruction]) -- The precompiled code of the function
              -> Compile CState ()
addFunDecl iden n rec c = do
  env <- funEnv
  modify $ \s -> s { functions = Scope.insert iden (rec, n, c) env}

-- | Creates a scope in the correct position where the parameters of a function should be looked up
insertParameters :: [Identifier] -> [Expr] -> Compile CState ()
insertParameters args values = do
  let newScope = foldl (flip $ uncurry Scope.insert) Scope.empty $ zip args values
  env <- varEnv
  let newEnv = father newScope env
  modify $ \s -> s { variables = newEnv }

-- | The identifier is looked up among the declared functions.
-- If the function is not in scope the monad fails with a 'CError'.
lookupFun :: Identifier -> Compile CState FunDef
lookupFun iden = do 
  env <- funEnv
  case Scope.lookup iden env of
    Just def -> return def 
    Nothing -> throwError $ FunNotInScope iden 

-- | The identifier is looked up among the scoped variables.
-- If the variable is not in scope the monad fails with a 'CError'.
lookupVar :: Identifier -> Compile CState Expr
lookupVar iden = do  
  env <- varEnv
  case Scope.lookup iden env of
    Just expr -> return expr
    Nothing -> throwError $ VarNotInScope iden

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
nextState = gets currentState

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

-- | @'setRecursiveCall' iden@ tracks that the tail recursive function @iden@ has already been once, at 
-- the current state.
setRecursiveCall :: Identifier -> Compile CState ()
setRecursiveCall iden = do 
  cs <- nextState
  rec <- recEnv
  modify $ \s -> s { recursive = M.insert iden cs rec } -- Possible old value is overwritten

-- | The program is now outside the recursive function.
-- The next call (if any) will reset the recursive call.
unsetRecursiveCall :: Identifier -> Compile CState ()
unsetRecursiveCall iden = do
  rec <- recEnv
  modify $ \s -> s { recursive = M.delete iden rec }

-- | Returns a @'Maybe' 'AntState'@ containing the 'AntState' if the recursive 
-- function has already been called, 'Nothing' otherwise.
getRecursiveCall :: Identifier -> Compile CState (Maybe AntState)
getRecursiveCall iden = do 
  rec <- recEnv
  return $ M.lookup iden rec


addLabel :: String -> AntState -> Compile CState ()
addLabel lbl as = modify $ \s -> s { labels = M.insert lbl as (labels s) }

lookupLabel :: String -> Compile CState AntState
lookupLabel lbl = gets ((M.! lbl) . labels)
