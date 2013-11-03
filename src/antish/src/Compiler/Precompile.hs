-- | This module takes care of handling those elements that cannot directly be 
-- compiled because some information is still missing.
-- For instance a function declaration cannot be directly compiled because 
-- its parameters values are not knonw until they are provide with a 
-- function call.

{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

module Compiler.Precompile (
    module Compiler.Class
  , precompileRecFun ) where

import Compiler.Class
import Compiler.Compile
import Compiler.Error
import Ast

instance Compilable c => PreCompilable c where
  precompile c argNames = \args -> do 
    insertParameters argNames args
    i <- compile c
    removeParameters
    return i

-- | Precompiles a recursive function, taking care of handling the recursive calls properly.
precompileRecFun :: Compilable StmBlock => Identifier    -- Name of the recursive procedure
                 -> StmBlock      -- Body of the procedure
                 -> ([Expr] -> Compile CState [Instruction])
precompileRecFun iden b = \args -> do
    setRecursiveCall iden
    i <- precompile b [] args
    unsetRecursiveCall iden
    return i
