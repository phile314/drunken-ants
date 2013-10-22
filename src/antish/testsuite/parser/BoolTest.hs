-- | This module tests the parser for Boolean (parenthesized) expressions 

module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Property 
import System.Exit
import Parser.Expr
import Text.Parsec
import BoolCombinators
import Ast

-- | The entry point for this test case
main = do 
  bst <- quickCheckResult boolExprSucceds
  if isSuccess bst
    then return ()
    else exitFailure

-- | Tests that fully parenthezied expressions are parsed properly.
boolExprSucceds :: BoolExpr -> Bool
boolExprSucceds expected = expected == actual 
  where input = stringOf expected
        actual = either (error . show) id $ parse pBoolExpr input input

-- | Parenthesized string representation of a Boolean Expression
stringOf :: BoolExpr -> String
stringOf (Not expr) = parenthesize $ "!" ++ stringOf expr 
stringOf (And expr1 expr2) = parenthesize $ stringOf expr1 ++ " && " ++ stringOf expr2 
stringOf (Or expr1 expr2) = parenthesize $ stringOf expr1 ++ " || " ++ stringOf expr2 
stringOf (Condition c s) = parenthesize $ unwords [show c, show s]

-- | Wraps a string with parenthesis
parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"
