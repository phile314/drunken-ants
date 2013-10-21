-- | Tests the 'For' construct

module ForTest where

import Test.HUnit
import Ast
import Compiler.Core
import Compiler.Compile
import Util

-- | The tests that will be run
forTests :: Test
forTests = TestLabel "For" $ TestList [emptyFor, simpleFor, moreInstructionBody]

-- | Tests a for-loop without any repetetion 
emptyFor :: Test
emptyFor = testCode expected input
  where expected = []
        input = compile $ For Nothing [] undefined

-- | Tests a simple for-loop with just one instruction in the body
simpleFor :: Test
simpleFor = testCode expected input
  where expected = [Turn IsLeft (s+1) | s <- [0..3]]
        input = compile $ For Nothing counter bodyFor
        counter = [ConstInt n | n <- [0..3]]
        bodyFor = StmBlock [TurnCall (CDir IsLeft)]

-- | Tests a for-loop with more instructions in the body
moreInstructionBody :: Test
moreInstructionBody = testCode expected input
  where expected = concat [bodyAss s | s <- [0,2,4]]
        bodyAss s = [Turn IsLeft (s+1), Turn IsLeft (s+2)]
        input = compile $ For Nothing counter bodyFor
        counter = [ConstInt n | n <- [1..3]]
        bodyFor = StmBlock [TurnCall (CDir IsLeft), TurnCall (CDir IsLeft)]
