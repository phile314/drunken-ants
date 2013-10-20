-- The Compiler module and the Compilable instances are tested here

module Main (main) where

import Test.HUnit
import System.Exit (exitFailure)
import Ast
import IfThenElseTest
import ErrorTest
import ForTest
import TryTest
import ProbTest

-- | The tests that will be run
tests :: Test
tests = TestLabel "Compiler" $ TestList [errorTests, ifThenElseTests, forTests, tryTests, withProbTests]

-- | The entry point of the test suite
main :: IO ()
main = do  
  result <- runTestTT tests
  if (failures result /= 0 || errors result /= 0)  
    then exitFailure
    else return ()
