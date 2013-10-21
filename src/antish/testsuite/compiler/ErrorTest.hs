-- | Defines tests cases for expected compilation failures with 'CError'.

module ErrorTest where

import Test.HUnit
import Ast
import Compiler.Core
import Compiler.Compile
import Compiler.Error
import Util

-- | The tests that will be run
errorTests :: Test
errorTests = TestLabel "CError" $ TestList [notInScope, wrongNumberParameters, invalidMarker]

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
        input = compile $ MarkCall (ConstInt 10)   -- TODO fix afer Integer -> Int change

-- | Tests that the proper 'CError' is returned when an invalid probability is used
invalidProbability :: Test
invalidProbability = testError expected input
  where invalidProb = 1.1
        expected = InvalidProbability invalidProb
        input = compile $ WithProb invalidProb undefined undefined
