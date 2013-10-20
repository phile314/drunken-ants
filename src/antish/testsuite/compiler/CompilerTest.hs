-- The Compiler module and the Compilable instances are tested here

module Main (main) where

import Test.HUnit
import System.Exit (exitFailure)
import Ast
import IfThenElseTest
import ErrorTest
import ForTest

tests :: Test
tests = TestLabel "Compiler" $ TestList [errorTests, ifThenElseTests, forTests]

-- | The entry point of the test suite
main :: IO ()
main = do  
  result <- runTestTT tests
  if (failures result /= 0 || errors result /= 0)  
    then exitFailure
    else return ()
