-- | This module provides a function to reduce expression replacing variables with the correspondent 
-- expression. It also checks that such expression is of the correct type.

module Compiler.Eval where

import Control.Monad
import Compiler.Error
import Compiler.Compile
import Ast (EType (..), Expr(..))
import Assembly

-- | @'eval' t e@ evaluates the expression @e@ and checks if it has type @t@.
-- If the expression contains unscoped variables, or variables of a wrong type a 'CError' is thrown.
eval :: EType -> Expr -> Compile CState Expr
eval EBool c@(Condition _ _) = return c
eval EBool (And c1 c2)       = liftM2 And (eval EBool c1) (eval EBool c2)
eval EBool (Or  c1 c2)       = liftM2 Or  (eval EBool c1) (eval EBool c2)
eval EBool (Not c)           = liftM Not (eval EBool c)
eval EInt  c@(ConstInt _)    = return c
eval EDir  c@(CDir _)        = return c
eval t v@(VarAccess iden)    = catchError (lookupVar iden >>= eval t) h
  where h (WrongType _ t' _) = throwError $ WrongType v t' t
        h e                  = throwError e
eval t e = do
  actual <- typeOf e 
  throwError $ WrongType e actual t

-- | Returns the type of an arbitrary expression
typeOf :: Expr -> Compile CState EType
typeOf (Condition _ _) = return EBool
typeOf (And _ _      ) = return EBool
typeOf (Or _ _       ) = return EBool
typeOf (VarAccess i  ) = lookupVar i >>= typeOf 
typeOf (CDir _       ) = return EDir
typeOf (ConstInt _   ) = return EInt

-- | Checks if the 'MarkerNumber' given is valid
validMarkerNumber :: MarkerNumber -> Bool
validMarkerNumber n = 0 <= n && n <= 5

-- | Checks if the number given is a vaild probability.
isProbability :: (Num a, Ord a) => a -> Bool
isProbability p = p > 0 && p <= 1
