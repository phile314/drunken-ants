-- The compilation of the IfThenElse construct is tested in this module

module IfThenElseTest where

import Test.HUnit
import Ast
import Compiler.Class
import Compiler.Compile
import Util

-- | The tests that will be run
ifThenElseTests :: Test
ifThenElseTests = TestLabel "IfThenElse" $ TestList [simpleIf, onlyIf]

-- | Tests a simple if-then-else
--
-- > if (Foe Here)
-- >   then Drop
-- >   else Mark
--
simpleIf = testCode expected input
  where expected = [Sense Here 1 2 Foe, Drop 3, Mark 1 3]
        input = compile simple
        simple = IfThenElse (Condition Foe Here) (StmBlock [DropCall]) (Just (StmBlock [MarkCall 1]))

-- | Only if branch, no else
--
-- > if (Foe Here)
-- >   then Drop
-- > Mark 1
--
onlyIf = testCode expected input
  where expected = [Sense Here 1 2 Foe, Drop 2, Mark 1 3]
        input = compile $ StmBlock [IfThenElse (Condition Foe Here) (StmBlock [DropCall]) Nothing, MarkCall 1]
