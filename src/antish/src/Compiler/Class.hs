-- | Defines the compiling operations for the elements of the Ast

module Compiler.Class where

import Assembly (Instruction, AntState) 

class Compilable a where
   -- | @'compile' b s@ returns a list of ant assembly instructions that correspond to the high-level
   -- entity $b$. The first instruction of the list will have AntState @b+1@.
   compile :: AntState -> a -> [Instruction]
