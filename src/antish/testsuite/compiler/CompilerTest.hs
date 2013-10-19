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

-- | Tests that the @'expected'@ error is returned when compiling @'input'@.
testError :: CError -> Compile CState [Instruction] -> Test
testError expected input = expected ~=? actual
  where actual = either id (error "Compilation should fail") result
        result = runCompile input empty

-- | Tests that the proper CError is returned when a non-scoped function is used
notInScope :: Test
notInScope = testError expected input 
  where foo = "foo"
        expected = FunNotInScope foo
        input = compile $ FunCall foo []

-- | Tests that the proper CError is returned when the wrong number of parameters is used
wrongNumberParameters :: Test
wrongNumberParameters = testError expected input
  where expected = WrongNumberParameters foo pActual pExpected
        input = (addFunDecl foo pExpected (\_ -> return [])) >> (compile $ FunCall foo [undefined])
        (foo, pExpected, pActual) = ("foo", 2, 1)

-- | Tests that the proper CError is returned when an invalid marker is used.
invalidMarker :: Test
invalidMarker = testError expected input
  where invalidMarkNumber = 10
        expected = InvalidMarkerNumber invalidMarkNumber
        input = compile $ MarkCall invalidMarkNumber

-- | Tests that the proper 'CError' is returned when an invalid probability is used
invalidProbability :: Test
invalidProbability = testError expected input
  where invalidProb = 1.1
        expected = InvalidProbability invalidProb
        input = compile $ WithProb invalidProb undefined undefined
