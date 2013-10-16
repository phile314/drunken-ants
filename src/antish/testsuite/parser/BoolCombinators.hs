-- | This module provides utility functions for tests related to boolean.

module BoolCombinators where

import Ast
import Assembly
import Test.QuickCheck
import Control.Monad

instance Arbitrary BoolExpr where
  arbitrary = oneof [ simpleBoolGen,
                      compoundBoolGen (expr 10)]

-- | Sized generator for boolean expression. It is guranteed to terminate
expr :: Int -> Gen BoolExpr
expr 0 = simpleBoolGen
expr n | n > 0 = oneof [simpleBoolGen, 
                         compoundBoolGen $ expr (n `div` 2)]

-- | Generates simple boolean condition
simpleBoolGen :: Gen BoolExpr
simpleBoolGen = liftM2 Condition arbitrary arbitrary

-- | Given a generator for simple boolean expression returns a generator for compound expressions,
-- namely with 'Or', 'And' and 'Not'
compoundBoolGen :: Gen BoolExpr -> Gen BoolExpr
compoundBoolGen gen = oneof [liftM2 And gen gen,
                             liftM2 Or gen gen,
                             liftM Not gen]

instance Arbitrary Cond where
  arbitrary = do 
    n <- elements [0..5]
    elements [Friend, Foe, FriendWithFood, FoeWithFood, Food, 
              Rock, FoeMarker, Home, FoeHome, Marker n]

instance Arbitrary SenseDir where
  arbitrary = elements [Here, Ahead, LeftAhead, RightAhead]

