-- | Provides high level features for compiling a 'Program'.

module Compiler (
  compile,
  module Compiler.Error
) where

import Ast
import Assembly
import Compiler.Compile
import qualified Compiler.Core as CO
import Compiler.Error

-- | Returns the assembly instructions representing the given program in low-level ant code.
-- If the program is not correct a 'CError' is returned.
compile :: Program -> Either CError [Instruction]
compile p = runCompile (CO.compile p) empty >>= return . fst
  
