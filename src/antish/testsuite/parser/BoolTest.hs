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
tests = TestList [simpleCondition] 

-- tests that all the basic conditions are parsed correctly
simpleCondition :: Test
simpleCondition = TestList $ [ expected x y ~=? actual x y | (x,y) <- bc]
   where bc = [(c,s) | c <- condition, s <- sensedir]
         sensedir = [Here, Ahead, LeftAhead, RightAhead]
         condition = [Friend, Foe, FriendWithFood, FoeWithFood, Food, Rock, 
                      FoeMarker, Home, FoeHome] ++ [Marker i | i <- [0..5]]
         expected x y = Condition x y
         actual x y = let input = show x ++ " " ++ show y
                          result = parse pBoolExpr input input in
                            either (error . show) id result
