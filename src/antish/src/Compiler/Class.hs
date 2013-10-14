-- | Defines the compiling operations for the elements of the Ast

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Class where

import Assembly (Instruction, AntState) 
import Compiler.Compile
import Ast
import Control.Monad.State hiding (gets)
import qualified Data.Map as Map
import Compiler.Error

-- | The state used in the Compile monad
data CState = CState {   currentState :: AntState -- ^ The first available state
                       , functions :: Map.Map Identifier FunDef
                       , variables :: Map.Map Identifier VarDef
                     } 

-- | 'FunDef' represents a function definition. The first element is the number of parameters and the second
-- is a function that given the exact number of parameters of the correct type returns the assembly code
-- for the function.
type FunDef = (Int, [Expr] -> Compile CState [Instruction])
type VarDef = Expr

-- | The empty CState
empty :: CState
empty = CState 0 Map.empty Map.empty

-- | Updates 'currentState' so that it points to an available state after producing some assembly code
update :: [Instruction] -> Compile CState ()
update xs = modify (\s -> s { currentState = (cs s) + length xs} )
  where cs s = currentState s

-- | Returns the current variable environment
varEnv :: Compile CState (Map.Map Identifier VarDef)
varEnv = do
  s <- get
  return $ variables s

-- | Returns the current function environment
funEnv :: Compile CState (Map.Map Identifier FunDef)
funEnv = do
  s <- get
  return $ functions s

-- | Inserts a binding in the proper environment
insertBinding :: Binding -> Compile CState ()
insertBinding (VarDecl iden expr) = do
  env <- varEnv
  modify $ \s -> s { variables = Map.insert iden expr env} 

insertBinding (FunDecl iden args b) = do
  env <- funEnv
  let c = precompile b args
  modify $ \s -> s { functions = Map.insert iden (length args, c) env}

-- | The identifier is looked up among the declared functions.
-- If the function is not in scope the monad throwErrors.
lookupFun :: Identifier -> Compile CState FunDef
lookupFun iden = do 
  env <- funEnv
  case Map.lookup iden env of
    Just def -> return def 
    Nothing -> throwError $ FunNotInScope iden 

insertParameters = undefined

class Compilable c where
  -- | Returns a monadic computation that performs the compilation
  compile :: c -> Compile CState [Instruction]
  
instance (Compilable a) => Compilable (Maybe a) where
  compile (Just x) = compile x
  compile Nothing = return []

instance (Compilable a) => Compilable [a] where
  compile xs = undefined

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
    mapM_ insertBinding bs 
    compile b

-- | This class represents something that can be preprocessed but some information has not been provided 
-- yet for it to be fully compiled, like for instance function declaration and variable access.
class PreCompilable c where
  precompile :: c -> [Identifier] -> ([Expr] -> Compile CState [Instruction])

instance PreCompilable StmBlock where
  precompile (StmBlock xs) argNames = \args -> do 
    insertParameters argNames args
    compile xs
