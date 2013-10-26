-- | This module tests the tail recursive recCall of recursive functions

module RecursiveTest where

import Test.HUnit
import Ast
import Compiler.Core
import Compiler.Compile
import Util

-- | The tests that will be run
recTests :: Test
recTests = TestLabel "Rec" $ TestList [simpleRec, doSomething, simpleMutualRec, moreMutualRec]

-- | Provides the Ast for a (recursive) function recCall, for the given function name
recCall :: Identifier -> Statement
recCall iden = FunCall iden []

-- | Provides the recursive function declaration with the given name
-- and the given body
recursive :: Identifier -> StmBlock -> Binding
recursive iden b = FunDecl Rec iden [] b

right :: Expr
right = CDir IsRight

left :: Expr
left = CDir IsLeft

-- | Tests a recursive function that simply recCalls itself over and over again
--
-- > rec f = f
--
simpleRec :: Test
simpleRec = testCode expected input
  where expected = [Flip 1 0 0]
        input = compile $ Let [recursive "f" body] (StmBlock [recCall "f"])
        body  = StmBlock [recCall "f"]

-- | Tests a recursive function that actually does something.
--
-- > rec f = { Turn Left;
-- >           f;   }
--
doSomething :: Test
doSomething = testCode expected input
  where expected = [Turn IsLeft 1, Flip 1 0 0]
        input    = compile $ Let [f] (StmBlock [recCall "f"])
        f        = recursive "f" (StmBlock body)
        body     = [TurnCall left, recCall "f"] 

-- | Tests two mutual recursive fuctions are compiled correctly
--
-- > rec f = { Turn Right, g }
-- >
-- > rec g = { Drop, f }
-- >
-- > main = f
--
simpleMutualRec :: Test
simpleMutualRec = testCode expected input
  where expected = [Drop 1, Turn IsRight 2, Flip 1 0 0]
        input    = compile $ Let [f, g] (StmBlock [recCall "f"])
        f        = recursive "f" (StmBlock [DropCall, recCall "g"])
        g        = recursive "g" (StmBlock [TurnCall right, recCall "f"])


-- | Tests that three mutual recursive functions are compiled correctly
--
-- > rec f = { Turn Right, g}
--
-- > rec g = { Turn Left, h}
--
-- > rec h = { Drop, f}
--
moreMutualRec :: Test
moreMutualRec = testCode expected input
  where expected = [Turn IsRight 1, Turn IsLeft 2, Drop 3, Flip 1 0 0]
        input    = compile $ Let [f, g, h] (StmBlock [recCall "f"])
        f        = recursive "f" (StmBlock [TurnCall right, recCall "g"])
        g        = recursive "g" (StmBlock [TurnCall left, recCall "h"])
        h        = recursive "h" (StmBlock [DropCall, recCall "f"])
