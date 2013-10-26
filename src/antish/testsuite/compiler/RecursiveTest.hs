-- | This module tests the tail recursive call of recursive functions

module RecursiveTest where

import Test.HUnit
import Ast
import Compiler.Core
import Compiler.Compile
import Util

-- | The tests that will be run
recTests :: Test
recTests = TestLabel "Rec" $ TestList [simpleRec]

-- | Tests a recursive function that simply calls itself over and over again
--
-- > rec f = f
--
simpleRec :: Test
simpleRec = testCode expected input
  where expected = [Flip 1 0 0]
        input = compile $ Let [FunDecl Rec "f" [] body] (StmBlock [FunCall "f" []])
        body  = StmBlock [FunCall "f" []]
