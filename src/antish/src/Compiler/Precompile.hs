-- | This module defines instances of the 'Precompile' class.

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Compiler.Precompile where

import Compiler.Class
import Compiler.Compile

instance Compilable c => PreCompilable c where  -- Check: Requires Undecidable instances
  precompile c argNames = \args -> do 
    insertParameters argNames args
    i <- compile c
    removeScope      -- Parameters Scope
    return i
