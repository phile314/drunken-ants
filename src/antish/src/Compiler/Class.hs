-- | Defines classes reflecting compilation features.

module Compiler.Class where

import Assembly (Instruction, AntState) 
import Compiler.Compile
import Ast

class Compilable c where
  -- | Returns a monadic computation that performs the compilation
  compile :: c -> Compile CState [Instruction]
  
------------------------------------------------------------------------------
-- Precombilable Definition

-- | This class represents a piece of code that can be preprocessed, and not 
-- directly compiled, because the value of some variables is not known yet. 
class Compilable c => PreCompilable c where
  precompile :: c 
             -> [Identifier]  -- ^ The value of these variables is not known beforehand
             -> ([Expr] -> Compile CState [Instruction]) -- ^ Given their value, @c@ can be compiled

-------------------------------------------------------------------------------
-- Jumpable Definition

-- | Represents some code that requires some jump in the control flow.
class Compilable c => Jumpable c where
  
  -- | @'compileWithJump' xs j@ compiles the 'Compiable' elements contained in 
  -- @xs@, so that on normal execution they are executed sequentially and after
  -- that the control flow jumps to @j@.
  compileWithJump :: c                          -- ^ What is going to be compied
                  -> (AntState -> AntState)     -- ^ Where to jump after this piece of code
                  -> Compile CState [Instruction]
