-- | This module tests the tail recursive call of recursive functions

module RecursiveTest where

import Test.HUnit
import Ast
import Compiler.Core
import Compiler.Compile
import Util

-- | The tests that will be run
recTests :: Test
recTests = TestLabel "Rec" $ TestList [simpleRec, doSomething, simpleMutualRec]

-- | Provides the Ast for a (recursive) function call, for the given function name
call :: Identifier -> Statement
call iden = FunCall iden []


-- | Tests a recursive function that simply calls itself over and over again
--
-- > rec f = f
--
simpleRec :: Test
simpleRec = testCode expected input
  where expected = [Flip 1 0 0]
        input = compile $ Let [FunDecl Rec "f" [] body] (StmBlock [FunCall "f" []])
        body  = StmBlock [FunCall "f" []]

-- | Tests a recursive function that actually does something.
--
-- > rec f = { Turn Left;
-- >           f;   }
--
doSomething :: Test
doSomething = testCode expected input
  where expected = [Turn IsLeft 1, Flip 1 0 0]
        input    = compile $ Let [f] (StmBlock [FunCall "f" []])
        f        = FunDecl Rec "f" [] (StmBlock body)
        body     = [TurnCall (CDir IsLeft), FunCall "f" []]

-- > rec f = { Turn Right, g }
-- >
-- > rec g = { Drop, f }
-- >
-- > main = f
--
simpleMutualRec :: Test
simpleMutualRec = testCode expected input
  where expected = [Drop 1, Turn IsRight 2, Flip 1 0 0]
        input    = compile $ Let [f, g] (StmBlock [call "f"])
        f        = FunDecl Rec "f" [] (StmBlock [DropCall, call "g"])
        g        = FunDecl Rec "g" [] (StmBlock [TurnCall (CDir IsRight), call "f"])
