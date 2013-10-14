-- | This module defines the compilation of the constructs of the language, i.e. how to compile them 
-- to low-lelel ant assembly.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.CompileT where

import Ast
import Assembly
import Control.Monad.State 
import Control.Monad.Error

type CError = String
 
newtype CompileT s m a = C (StateT s (ErrorT CError m) a)

instance MonadTrans (CompileT s) where
  lift = C . lift . lift

instance (Monad m) => Monad (CompileT s m) where
  return = C . return
  C f >>= g = C $ do
    x <- f
    let C y = g x in y
  fail = C . lift . throwError

instance (Monad m) => MonadState s (CompileT s m) where
  get = C $ get
  put = C . put

runCompileT :: (Monad m) => s -> CompileT s m a -> m (Either CError (a,s))
runCompileT s (C st) = runErrorT $ runStateT st s 
