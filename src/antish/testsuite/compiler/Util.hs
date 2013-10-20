-- | This module contains some general purpose utility testing functions

module Util where

import Compiler.Error
import Compiler.Compile
import Ast
import Test.HUnit

-- | Tests that the @'expected'@ error is returned when compiling @'input'@.
testError :: CError -> Compile CState [Instruction] -> Test
testError expected input = expected ~=? actual
  where actual = either id (error "Compilation should fail") result
        result = runCompile input empty

-- | Tests that the assembly code compiled is correct 
testCode :: [Instruction]                  -- ^ Expected assembly code
         -> Compile CState [Instruction]   -- ^ Will be run and compared with the expected assembly code
         -> Test
testCode expected input = expected ~=? actual                             
  where actual = either (error "Compilation should succeed") fst result         
        result = runCompile input empty


