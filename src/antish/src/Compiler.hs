module Compiler (
  compile
) where

import Ast
import Assembly

-- | Returns the assembly instructions representing the given program in low-level ant code.
compile :: Program -> AssemblyFrag
compile = undefined
