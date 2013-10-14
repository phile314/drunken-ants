-- | This module defines the compilation of the constructs of the language, i.e. how to compile them 
-- to low-lelel ant assembly.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.CompileT where

import Ast
import Assembly
import Control.Monad.State
import Control.Monad.Error

type CState s = s -- We might change this later on
type CError = String
 
newtype CompileT s m a = C (StateT (CState s) (ErrorT CError m) a)

instance MonadTrans (CompileT s) where
  lift = C . lift . lift

instance (Monad m) => Monad (CompileT s m) where
  return = C . return
  C f >>= g = C $ do
    x <- f
    let C y = g x in y

instance (Monad m) => MonadState s (CompileT s m) where
  get = C $ get
  put = C . put

runCompileT :: (Monad m) => s -> CompileT s m a -> m (Either CError (a,s))
runCompileT s (C st) = runErrorT $ runStateT st s 

first :: CompileT s m AntState
first = undefined

-- length :: CompileT s m Int
-- length = undefined

last :: CompileT s m AntState 
last = undefined

{- How to make CompileT instance of ErrorT ? Should we?
instance (Monad m) => MonadError CError (CompileT s m) where
  throwError = C . lift throwError
  catchError = undefined
-}
