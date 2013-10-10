-- Tests for boolean exppressions parser

module Main where

import Test.HUnit
import System.Exit (exitFailure)

-- | The entry point of the test suite
main :: IO ()
main = do  
  result <- runTestTT tests
  if (failures result /= 0 || errors result /= 0)  
    then exitFailure
    else return ()

tests :: Test
tests = TestList [] 

simpleCondition :: Test
simpleCondition = undefined
