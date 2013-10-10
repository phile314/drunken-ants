-- Tests for boolean exppressions parser

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

-- | The tests that will be run
tests :: Test
tests = TestList [simpleCondition, simpleNotCondition, simpleOrCondition, simpleAndCondition] 

-- | Returns a list containing all the 'Cond' and 'SenseDir' pairs, that are
-- | all the basic conditions that can be tested in a @if@
allBasicInput :: [(Cond,SenseDir)]
allBasicInput = [(c,s) | c <- condition, s <- sensedir]
  where sensedir = [Here, Ahead, LeftAhead, RightAhead]
        condition = [Friend, Foe, FriendWithFood, FoeWithFood, Food, Rock, 
                     FoeMarker, Home, FoeHome] ++ [Marker i | i <- [0..5]]


-- | Check for every input generated by 'allBasicInput' the equality of the returning values of 
-- expected and actual
testAllWith :: (Eq a, Show a) =>
     (Cond -> SenseDir -> a) -> -- | The function that returns the expected value
     (Cond -> SenseDir -> a) -> -- | The function that returns the actual value
      Test
testAllWith expected actual = TestList $ [expected x y ~=? actual x y | (x,y) <- allBasicInput]

-- | Check for all couple of conditions generated by 'allBasicInput' the equality of the returning values of 
-- expected and actual
testAllWith2 :: (Eq a, Show a) =>
     ((Cond,SenseDir) -> (Cond,SenseDir) -> a) -> -- | The function that returns the expected value
     ((Cond,SenseDir) -> (Cond,SenseDir) -> a) -> -- | The function that returns the actual value
      Test
testAllWith2 expected actual = TestList $ [expected c1 c2 ~=? actual c1 c2 | c1 <- allBasicInput, 
                                                                             c2 <- allBasicInput]

-- | Tests that all the basic conditions are parsed correctly
simpleCondition :: Test
simpleCondition = testAllWith (Condition) (actual)
   where actual x y = either (error . show) id result
          where result = parse pBoolExpr input input 
                input = show x ++ " " ++ show y

-- | Tests that the use of @!@ before a boolean condition is handled correctly
simpleNotCondition :: Test
simpleNotCondition = testAllWith expected actual
  where expected x y = Not $ Condition x y
        actual x y   = either (error . show) id result 
          where result = parse pBoolExpr input input 
                input = "!" ++ show x ++ " " ++ show y

-- | Tests that the use of @&&@ between two boolean conditions is handled correctly
simpleAndCondition :: Test
simpleAndCondition = testAllWith2 expected actual
  where expected (c1,s1) (c2,s2) = And (Condition c1 s1) (Condition c2 s2)
        actual (c1,s1) (c2,s2)   = either (error . show) id result 
          where result = parse pBoolExpr input input
                input  = show c1 ++ " " ++ show s1 ++ " && " ++ show c2 ++ " " ++ show s2

-- | Tests that the use of @||@ between two boolean conditions is handled correctly
simpleOrCondition :: Test
simpleOrCondition = testAllWith2 expected actual
  where expected (c1,s1) (c2,s2) = Or (Condition c1 s1) (Condition c2 s2)
        actual (c1,s1) (c2,s2)   = either (error . show) id result 
          where result = parse pBoolExpr input input
                input  = show c1 ++ " " ++ show s1 ++ " || " ++ show c2 ++ " " ++ show s2
