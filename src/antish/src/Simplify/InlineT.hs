{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simplify.InlineT where

import Ast
import Assembly
import Control.Monad.State
import Control.Monad.Error
import Compiler.Error as Error
import Control.Monad.Identity

type Inline a = InlineT Int Identity a

newtype InlineT s m a = C (StateT s (ErrorT TError m) a)

instance MonadTrans (InlineT s) where
  lift = C . lift . lift

instance Monad m => Monad (InlineT s m) where
  return = C . return
  C f >>= g = C $ do
    x <- f
    let C y = g x in y

instance Monad m => MonadState s (InlineT s m) where
  get = C $ get
  put = C . put

throwError :: Monad m => TError -> InlineT s m a
throwError = C . lift . Control.Monad.Error.throwError

runInlineT :: (Monad m) => InlineT s m a -> s -> m (Either TError (a,s))
runInlineT (C st) s = runErrorT $ runStateT st s

data TError
  = RecursionLimitReached Int
  | OtherError String
  deriving (Show)

instance Error TError where
  strMsg s = OtherError s
