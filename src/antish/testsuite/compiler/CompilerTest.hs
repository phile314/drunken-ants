-- The Compiler module and the Compilable instances are tested here

module Main (main) where

import Test.HUnit
import System.Exit (exitFailure)
import Compiler.Error 
import Ast
import Compiler.Class
import Compiler.Compile

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
  where expected = FunNotInScope foo
        actual = either id (error "Compilation should fail") result
        input = FunCall foo []
        result = runCompile (compile input) empty
        foo = "foo"

-- | Tests that the proper CError is returned when the wrong number of parameters is used
wrongNumberParameters :: Test
wrongNumberParameters = expected ~=? actual
  where expected = WrongNumberParameters foo pActual pExpected
        actual = either id (error "Compilation should fail") result
        input = (addFunDecl foo pExpected (\_ -> return [])) >> (compile $ FunCall foo [undefined])
        result = runCompile input empty
        (foo, pExpected, pActual) = ("foo", 2, 1)

-- | Tests that the proper CError is returned when an invalid marker is used.
invalidMarker :: Test
invalidMarker = expected ~=? actual
  where expected = InvalidMarkerNumber invalidMarkNumber
        actual = either id (error "Compilation should fail") result
        input = MarkCall invalidMarkNumber
        result = runCompile (compile input) empty
        invalidMarkNumber = 10
