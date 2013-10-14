module Compiler.Compile (
    module Compiler.CompileT
  , module Compiler.Compile ) -- TODO remove§
   where

import Control.Monad.Identity
import Compiler.CompileT
import Assembly
import Control.Monad.State
import Compiler.Error

type Compile s a = CompileT s Identity a

runCompile :: s -> Compile s a -> Either CError (a ,s)
runCompile s c = runIdentity $ runCompileT s c
