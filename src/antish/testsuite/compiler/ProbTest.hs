-- | This module tests the 'WithProb' construct compilation
module ProbTest where

import Test.HUnit
import Ast
import Compiler.Core
import Compiler.Compile
import Util

-- | The tests that will be run
withProbTests :: Test
withProbTests = TestLabel "Prob" $ TestList [simpleProb]

-- | Tests a simple WithProb statement
--
-- > WithProb 0.5 {
-- >    Turn Left
-- > } {
-- >    Turn Right
-- > }
-- > Mark 1
simpleProb :: Test
simpleProb = testCode expected input
  where expected = [Flip 2 1 2, Turn IsLeft 3, Turn IsRight 3, Mark 1 4]
        input = compile $ StmBlock [WithProb 0.5 b1 b2, MarkCall (ConstInt 1)]
        b1 = StmBlock [TurnCall IsLeft]
        b2 = StmBlock [TurnCall IsRight]
