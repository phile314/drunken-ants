-- | Defines classes reflecting compilation features.

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Class where

import Assembly (Instruction, AntState) 
import Compiler.Compile
import Ast

class Compilable c where
  -- | Returns a monadic computation that performs the compilation
  compile :: c -> Compile CState [Instruction]
  
------------------------------------------------------------------------------
-- Precombilable Definition

-- | This class represents a piece of code that can be preprocessed, and not directly compiled,
-- because the value of some variables is not known yet. 
class PreCompilable c where
  precompile :: c -> [Identifier] -> ([Expr] -> Compile CState [Instruction])

-------------------------------------------------------------------------------
-- Jumpable Definition

-- | Represents some code that requires some jump in the control flow
class Compilable c => Jumpable c where
  
  -- | @'compileWithJump' xs j@ compiles the 'Compiable' elements contained in @xs@, so that on normal execution
  -- they are executed sequentially and after that the control flow jumpt to @j@.
  compileWithJump :: c                          -- ^ What is going to be compied
                  -> (AntState -> AntState)     -- ^ Where to jump after this piece of code
                  -> Compile CState [Instruction]
