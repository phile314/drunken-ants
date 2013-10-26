-- | This module takes care of handling those elements that cannot directly be 
-- compiled because some information is still missing.
-- For instance a function declaration cannot be directly compiled because 
-- its parameters instances are not knonw until a function call is provided.

{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

module Compiler.Precompile (
    module Compiler.Class
  , precompileRecFun ) where

import Compiler.Class
import Compiler.Compile
import Compiler.Error
import Ast

instance Compilable c => PreCompilable c where  -- Check: Requires Undecidable instances
  precompile c argNames = \args -> do 
    insertParameters argNames args
    i <- compile c
    removeScope      -- Parameters Scope
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
