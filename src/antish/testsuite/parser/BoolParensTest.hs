-- | This module tests Boolean parenthesized expressions

{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Test.QuickCheck
import System.Exit
import Test.QuickCheck.Test
import Ast
import Assembly
import Parser.Boolean
import Text.Parsec
import Test.QuickCheck.Property 

-- | The entry point for this test case
main = do 
  results <- quickCheckResult $ forAll parens boolParsSucceds
  if isSuccess results
    then return ()
    else exitFailure

boolParsSucceds :: (BoolExpr, String) -> Bool
boolParsSucceds (expected, input) = expected == actual
  where actual = either (error . show) id $ parse pBoolExpr input input

instance Arbitrary BoolExpr where
  arbitrary = compoundBoolGenerator

-- | Generates a simple Boolean Expression
simpleBoolGenerator :: Gen BoolExpr
simpleBoolGenerator = do
  n <- elements [0..5]
  cond <- elements [Friend, Foe, FriendWithFood, FoeWithFood,
                 Food, Rock, FoeMarker, Home, FoeHome, Marker n]
  dir <- elements [Here, Ahead, LeftAhead, RightAhead]
  return $ Condition cond dir

-- | Generates also compound boolean expressions
compoundBoolGenerator :: Gen BoolExpr
compoundBoolGenerator = do
  compound <- arbitrary
  expr1 <- simpleBoolGenerator
  expr2 <- simpleBoolGenerator
  if not compound then elements [expr1, expr2]
    else combine expr1 expr2 

-- | Randomly combines two given boolean expressions
-- It may returns expr1 && expr2, expr1 || expr2, !expr1, !expr2
combine :: BoolExpr -> BoolExpr -> Gen BoolExpr
combine expr1 expr2 = elements [And expr1 expr2, Or expr1 expr2, Not expr1, Not expr2]

-- | Returns a boolean expression and a randomly parenthesized string representation of it
parens :: Gen (BoolExpr, String) 
parens = do
    expr <- arbitrary
    p <- arbitrary
    if not p then return (expr, stringOf expr)
      else return (expr, "(" ++ stringOf expr ++ ")") 

-- | Unparenthesized string representation of a Boolean Expression
stringOf :: BoolExpr -> String
stringOf (Not expr) = "! " ++ stringOf expr
stringOf (And expr1 expr2) = stringOf expr1 ++ " && " ++ stringOf expr2
stringOf (Or expr1 expr2) = stringOf expr1 ++ " || " ++ stringOf expr2
stringOf (Condition c s) = unwords [show c, show s]
