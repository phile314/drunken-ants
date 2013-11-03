-- This module is concerned with the compilation of built-in functions

module Compiler.BuiltIn ( 
    safeFunCall
  , unsafeFunCall
  , compileWithMarker ) where

import Ast
import Compiler.Eval
import Compiler.Compile
import Compiler.Error

-- | Compiles a safe function call (i.e. it cannot fail). 
safeFunCall :: (AntState -> Instruction) -> Compile CState [Instruction]
safeFunCall f = do
  s <- goNext
  generate [f s]
 
 
-- | Compiles an unsafe function call. 'onFailure' field from the state is used.
unsafeFunCall :: 
  (AntState -> AntState -> Instruction) -- ^ The first argument is where to jump on normal execution, the second is where to jump on failure 
  -> Compile CState [Instruction]
unsafeFunCall f = do
  normal      <- goNext 
  failure     <- getOnFailure
  generate [f normal failure]


-- | Performs the compilation only if the given 'Expr' is a valid 'MarkerNumber',
-- otherwise it throws the 'CError' 'InvalidMarkerNumber'.
compileWithMarker :: (MarkerNumber -> Compile CState [Instruction])
                  -> Expr
                  -> Compile CState [Instruction]
compileWithMarker f e = do
  ConstInt n <- eval EInt e
  if validMarkerNumber n 
      then f n
      else throwError $ InvalidMarkerNumber n
