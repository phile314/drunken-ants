-- | Defines the compiling operations for the elements of the Ast

module Compiler.Class where

import Assembly (Instruction, AntState) 
import Compiler.Compile
import Ast
import Control.Monad.State hiding (gets)
import qualified Data.Map as Map

-- | The state used in the Compile monad
data CState = CState {   currentState :: AntState -- ^ The first available state
                       , functions :: Map.Map Identifier Definition
                     } 

-- | 'FunDef' represents a function definition. The first element is the number of parameters and the second
-- is a function that given the exact number of parameters of the correct type returns the assembly code
-- for the function.
type Definition = (Int, [Expr] -> [Instruction])

-- | The empty CState
empty :: CState
empty = CState 0 Map.empty

-- | Updates 'currentState' so that it points to an available state after producing some assembly code
update :: [Instruction] -> Compile CState ()
update xs = modify (\s -> s { currentState = (cs s) + length xs} )
  where cs s = currentState s

-- | The identifier is looked up among the declared functions.
-- If the function is not in scope the monad fails.
lookupFun :: Identifier -> Compile CState Definition
lookupFun iden = do 
  s <- get 
  let funMap = functions s 
  case Map.lookup iden funMap of
    Just def -> return def 
    Nothing -> fail $ "Function " ++ iden ++ " is not in scope"

class Compilable c where
   compile :: c -> Compile CState [Instruction]

instance (Compilable a) => Compilable (Maybe a) where
  compile (Just x) = compile x
  compile Nothing = undefined

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
--    _ -> fail (show expr ++ " is not a boolean expression") -- BoolExpr will be part of Expr soon

  compile (FunCall ident args) = do 
    (nargs, body) <- lookupFun ident
    if nargs /= length args then fail $ ident ++ " expects " ++ show (nargs) ++ " arguments"
      else return $ body args
 
