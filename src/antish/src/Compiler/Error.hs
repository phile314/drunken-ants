-- | This modules defines the data type 'CError' used for throwing error in the 'Compile' monad.
module Compiler.Error where

import Ast
import Control.Monad.Error

data CError = FunNotInScope Identifier -- ^ A function used is not in scope
            | NotBoolean Expr          -- ^ The expression is not a boolean expression
            | WrongNumberParameters Identifier Int Int -- ^ The number of parameters used is wrong (first number given, second number expected
            | AnyError String
    deriving Eq

instance Error CError where
  strMsg s = AnyError s

instance Show CError where
  show (FunNotInScope iden ) = "The function " ++ show iden ++ " is not in scope"
  show (NotBoolean expr) = "The expression " ++ show expr ++ " is not Boolean"
  show (WrongNumberParameters iden actual expected) = required ++ given 
    where required = iden ++ " requires " ++ show expected
          given    = " (" ++ show actual ++ " given)"


