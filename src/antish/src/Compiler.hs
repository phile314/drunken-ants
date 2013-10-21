module Compiler (
  compile,
  module Compiler.Error
) where

import Ast
import Assembly
import Compiler.Compile
import Compiler.Error

-- | Returns the assembly instructions representing the given program in low-level ant code.
compile :: Program -> Either CError [Instruction]
compile p = compile p
