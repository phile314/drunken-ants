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

runCompile :: Compile s a -> s -> Either CError (a ,s)
runCompile c s = runIdentity $ runCompileT c s
