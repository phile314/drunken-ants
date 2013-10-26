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
import ScopeTest
import RecursiveTest

-- | The tests that will be run
tests :: Test
tests = TestLabel "Compiler" $ TestList tests
  where tests = [errorTests, ifThenElseTests, forTests, tryTests, withProbTests, scopeTests, recTests]

-- | The entry point of the test suite
main :: IO ()
main = do  
  result <- runTestTT tests
  if (failures result /= 0 || errors result /= 0)  
    then exitFailure
    else return ()
