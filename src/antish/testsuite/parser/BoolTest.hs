-- Tests for boolean exppressions parser

{-# LANGUAGE BangPatterns #-}

module Main where

import Test.HUnit
import System.Exit (exitFailure)
import Assembly (Cond (..), SenseDir (..))
import Parser.Boolean
import Ast
import Text.Parsec
import Data.Either 

-- | The entry point of the test suite
main :: IO ()
main = do  
  result <- runTestTT tests
  if (failures result /= 0 || errors result /= 0)  
    then exitFailure
    else return ()

tests :: Test
tests = TestList [simpleCondition, simpleNotCondition] 

-- | Returns a list containing all the 'Cond' and 'SenseDir' pairs, that are
-- | all the basic conditions that can be tested in a @if@
allBasicInput :: [(Cond,SenseDir)]
allBasicInput = [(c,s) | c <- condition, s <- sensedir]
  where sensedir = [Here, Ahead, LeftAhead, RightAhead]
        condition = [Friend, Foe, FriendWithFood, FoeWithFood, Food, Rock, 
                     FoeMarker, Home, FoeHome] ++ [Marker i | i <- [0..5]]


-- | Tests that all the basic conditions are parsed correctly
simpleCondition :: Test
simpleCondition = TestList $ [expected x y ~=? actual x y | (x,y) <- allBasicInput]
   where expected = Condition 
         actual x y = let input = show x ++ " " ++ show y
                          result = parse pBoolExpr input input in
                            either (error . show) id result

-- | Tests that the use of not (@!@) before a boolean condition is handled correctly
simpleNotCondition :: Test
simpleNotCondition = TestList $ [expected x y ~=? actual x y | (x,y) <- allBasicInput]
  where expected x y = Not $ Condition x y
        actual x y = let input = "!" ++ show x ++ " " ++ show y
                         result = parse pBoolExpr input input in
                          either (error . show) id result 

