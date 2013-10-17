-- The Compiler module and the Compilable instances are tested here

module Main (main) where

import Test.HUnit
import System.Exit (exitFailure)
import Compiler.Error 
import Ast
import Compiler.Class
import Compiler.Compile
import qualified Compiler.Scope as Scope

-- | The entry point of the test suite
main :: IO ()
main = do  
  result <- runTestTT tests
  if (failures result /= 0 || errors result /= 0)  
    then exitFailure
    else return ()

-- | The tests that will be run
tests :: Test
tests = TestList [notInScope, wrongNumberParameters, invalidMarker]

-- | Tests that the proper CError is returned when a non-scoped function is used
notInScope :: Test
notInScope = expected ~=? actual
  where input = FunCall foo []
        foo = "foo"
        expected = FunNotInScope foo
        actual = either id (error "Compilation should fail") result
        result = runCompile (compile input) empty

-- | Tests that the proper CError is returned when the wrong number of parameters is used
wrongNumberParameters :: Test
wrongNumberParameters = expected ~=? actual
  where input = FunCall foo [undefined]
        foo = "foo"
        expected = WrongNumberParameters foo 1 2
        actual = either id (error "Compilation should fail") result
        result = runCompile (compile input) (CState 0 funEnv Scope.empty (+1) [0])
        funEnv = Scope.insert foo (2, undefined) Scope.empty 

-- | Tests that the proper CError is returned when an invalid marker is used.
invalidMarker :: Test
invalidMarker = expected ~=? actual
  where invalidMarkNumber = 10
        input = MarkCall invalidMarkNumber
        expected = InvalidMarkerNumber invalidMarkNumber
        actual = either id (error "Compilation should fail") result
        result = runCompile (compile input) empty
