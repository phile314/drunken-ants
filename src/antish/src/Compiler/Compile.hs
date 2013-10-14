module Compiler.Compile where

import Control.Monad.Identity
import Compiler.CompileT
import Assembly
import Control.Monad.Error
import Control.Monad.State

type Compile s a = CompileT s Identity a

runCompile :: s -> Compile s a -> Either CError (a ,s)
runCompile s c = runIdentity $ runCompileT s c
