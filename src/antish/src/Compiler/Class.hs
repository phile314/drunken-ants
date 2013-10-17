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

-- | The state used in the Compile monad
data CState = CState {   currentState :: AntState -- ^ The first available state
                       , functions :: Scope Identifier FunDef -- ^ The functions scope environment
                       , variables :: Scope Identifier VarDef -- ^ The variables scope environment
                       , jumpTo    :: (AntState -> AntState)  -- ^ Given the current state returns the state where to jump after the current instruction have been executed
                       , onFailure :: (Maybe AntState)        -- ^ Where to jump on failure
                     } 

-- | 'FunDef' represents a function definition. The first element is the number of parameters and the second
-- is a function that given the exact number of parameters of the correct type returns the assembly code
-- for the function.
type FunDef = (Int, [Expr] -> Compile CState [Instruction])
type VarDef = Expr

-- | The empty CState
empty :: CState
empty = CState 0 Scope.empty Scope.empty (+1) (Just 0)

-- | Updates 'currentState' so that it points to an available state after producing some assembly code
update :: [Instruction] -> Compile CState ()
update xs = modify (\s -> s { currentState = (cs s) + length xs} )
  where cs s = currentState s

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
        update i1
        i2 <- compile b2
        let res = [Sense sd 0 0 c] ++ i1 ++ i2  -- TODO missing correct jump
        update res
        return res
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

  compile (MarkCall n) | 0 <= n && n <= 5 = do
    s <- get
    let current = currentState s
        result = [Mark n current]
    update result
    return result

  compile (MarkCall n) = throwError $ InvalidMarkerNumber n

-------------------------------------------------------------------------------
-- | This class represents something that can be preprocessed but some information has not been provided 
-- yet for it to be fully compiled, like for instance function declaration and variable access.
class PreCompilable c where
  precompile :: c -> [Identifier] -> ([Expr] -> Compile CState [Instruction])

instance PreCompilable StmBlock where
  precompile (StmBlock xs) argNames = \args -> do 
    insertParameters argNames args
    i <- compile xs
    removeScope      -- Parameters Scope
    removeScope      -- Block Scope -- TODO should be removed by block
    return i
    
