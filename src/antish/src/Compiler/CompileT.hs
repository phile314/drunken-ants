-- | This module defines the compilation of the constructs of the language, i.e. how to compile them 
-- to low-lelel ant assembly.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.CompileT where

import Ast
import Assembly
import Control.Monad.State 
import Control.Monad.Error
import Compiler.Error as Error

newtype CompileT s m a = C (StateT s (ErrorT CError m) a)

instance MonadTrans (CompileT s) where
  lift = C . lift . lift

instance Monad m => Monad (CompileT s m) where
  return = C . return
  C f >>= g = C $ do
    x <- f
    let C y = g x in y

instance Monad m => MonadState s (CompileT s m) where
  get = C $ get
  put = C . put

throwError :: Monad m => CError -> CompileT s m a
throwError = C . lift . Control.Monad.Error.throwError

runCompileT :: (Monad m) => CompileT s m a -> s -> m (Either CError (a,s))
runCompileT (C st) s = runErrorT $ runStateT st s 
