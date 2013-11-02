-- | This modules defines the data type 'CError' used for throwing error in the 'Compile' monad.

module Compiler.Error where

import Ast
import Tree
import Control.Monad.Error

data CError = 
  FunNotInScope Identifier -- ^ A function used is not in scope
 | VarNotInScope Identifier -- ^ A variable used is not in scope
 | NotBoolean Expr          -- ^ The expression is not a boolean expression
 | WrongNumParam Identifier Int Int -- ^ The number of parameters used is wrong (first given, second expected)
 | InvalidMarkerNumber Int        -- ^ An invalid marker has been used
 | InvalidProbability Double      -- ^ An invalid probability has been used
 | WrongType Expr EType EType     -- ^ An expression has not the correct type
 | NotTailRecursive Identifier StmBlock  -- ^ The function is not tail recursive
 | InvalidRecFun Identifier [Identifier]  -- ^ A recursive function cannot have arguments
 | AnyError String          -- ^ A generic error
    deriving Eq

instance Error CError where
  strMsg s = AnyError s

instance Show CError where
  show (FunNotInScope iden ) = "The function " ++ show iden ++ " is not in scope"

  show (VarNotInScope iden ) = "The variable " ++ show iden ++ " is not in scope"

  show (NotBoolean expr) = "The expression " ++ show expr ++ " is not Boolean"

  show (InvalidMarkerNumber n) = "Invalid Marker: " ++ show n ++ ". Must be one of 0, 1, 2, 3, 4, 5"

  show (InvalidProbability p) = "A probability must be included in (0, 1] (" ++ show p ++ " given)"

  show (WrongType e t1 t2) = what ++ actual ++ expected 
    where what = "The expression " ++ show e ++ " has type "
          actual = show t1
          expected = " (expected " ++ show t2 ++ ")"

  show (WrongNumParam iden actual expected) = required ++ given 
    where required = iden ++ " requires " ++ show expected
          given    = " (" ++ show actual ++ " given)"

  show (NotTailRecursive iden b) = what ++ why 
    where what = "The function " ++ iden ++ " is not tail-recursive."
          why = "\nIts body is\n" ++ drawAst b

  show (InvalidRecFun iden xs) = iden ++ ": invalid recursive function declaration, " ++ what
    where what = "no parameters can be passed, (" ++ show (length xs) ++ " given)\n"
