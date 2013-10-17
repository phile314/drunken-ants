-- | Defines the compiling operations for the elements of the Ast

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Class where

import Assembly (Instruction, AntState) 
import Compiler.Compile
import Ast
import Control.Monad.State hiding (gets)
import Compiler.Error
import Compiler.Scope as Scope
import Data.Maybe

-- | The state used in the Compile monad
data CState = CState {   currentState :: AntState -- ^ The first available state
                       , functions :: Scope Identifier FunDef -- ^ The functions scope environment
                       , variables :: Scope Identifier VarDef -- ^ The variables scope environment
                       , jumpTo    :: (AntState -> AntState)  -- ^ Given the current state returns the state where to jump after the current instruction have been executed
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

-- | Inserts a binding in the proper environment
insertBinding :: Binding -> Compile CState ()
insertBinding (VarDecl iden expr) = do
  env <- varEnv
  modify $ \s -> s { variables = Scope.insert iden expr env} 
insertBinding (FunDecl iden args b) = do
  env <- funEnv
  let c = precompile b args
  modify $ \s -> s { functions = Scope.insert iden (length args, c) env}

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

class Compilable c where
  -- | Returns a monadic computation that performs the compilation
  compile :: c -> Compile CState [Instruction]
  
instance (Compilable a) => Compilable (Maybe a) where
  compile (Just x) = compile x
  compile Nothing = return []

instance (Compilable a) => Compilable [a] where
  compile xs = do
      i <- mapM compile xs
      return $ concat i

instance Compilable Program where
  compile (Program smb) = compile smb

instance Compilable StmBlock where
  compile = undefined

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

  compile (MarkCall n) | validMarkerNumber n  = safeFunCall (Mark n)
  compile (MarkCall n) = throwError $ InvalidMarkerNumber n

  compile (UnMarkCall n) | validMarkerNumber n = safeFunCall (Unmark n)
  compile (UnMarkCall n) = throwError $ InvalidMarkerNumber n

  compile DropCall = safeFunCall Drop

  compile MoveCall = unsafeFunCall Move

  compile (TurnCall d) = safeFunCall (Turn d)

-- | Compiles a safe function call (i.e. it cannot fail). After the function call the next instruction is 
-- executed.
safeFunCall :: (AntState -> Instruction) -> Compile CState [Instruction]
safeFunCall f = do
  s <- get >>= (return . currentState)
  generate [f s]
  
-- | Compiles an unsafe function call. 'onFailure' field from the state is used.
unsafeFunCall :: (AntState ->  -- ^ Where to jump on normal execution
                  AntState ->  -- ^ Where to jump on failure
                  Instruction) -> Compile CState [Instruction]
unsafeFunCall f = do
  normal      <- get >>= (return . currentState)
  (failure:_) <- get >>= (return . onFailure)
  generate [f normal failure]

-------------------------------------------------------------------------------
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
-- | Checks if the 'MarkerNumber' given is valid
validMarkerNumber :: MarkerNumber -> Bool
validMarkerNumber n = 0 <= n && n <= 5
