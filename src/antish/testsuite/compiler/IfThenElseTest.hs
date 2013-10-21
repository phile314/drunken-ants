-- The compilation of the IfThenElse construct is tested in this module

module IfThenElseTest where

import Test.HUnit
import Ast
import Compiler.Core
import Compiler.Compile
import Util

-- | The tests that will be run
ifThenElseTests :: Test
ifThenElseTests = TestLabel "IfThenElse" $ TestList tests
  where tests = [simpleIf, onlyIf, notCond, onlyIfNot, andCond, onlyIfAnd, orCond, onlyIfOr]

-- | Tests a simple if-then-else
--
-- > if (Foe Here)
-- >   then Drop
-- >   else Mark
--
simpleIf = testCode expected input
  where expected = [Sense Here 1 2 Foe, Drop 3, Mark 1 3]
        input = compile simple
        simple = IfThenElse (Condition Foe Here) bThen bElse
        bThen = StmBlock [DropCall]
        bElse = StmBlock [MarkCall (ConstInt 1)]

-- | Only if branch, no else
--
-- > if (Foe Here)
-- >   then Drop
-- > Mark 1
--
onlyIf = testCode expected input
  where expected = [Sense Here 1 2 Foe, Drop 2, Mark 1 3]
        input = compile $ StmBlock [ifte, MarkCall (ConstInt 1)]
        ifte = IfThenElse (Condition Foe Here) bThen bElse
        bThen = StmBlock [DropCall]
        bElse = StmBlock []

-- | Tests a simple if-then-else
--
-- > if !(Foe Here)
-- >   then Drop
-- >   else Mark
--
notCond = testCode expected input
  where expected = [Sense Here 2 1 Foe, Drop 3, Mark 1 3]
        input = compile simpleNot
        simpleNot = IfThenElse (Not (Condition Foe Here)) bThen bElse
        bThen = StmBlock [DropCall]
        bElse = StmBlock [MarkCall (ConstInt 1)]

-- | Only if branch, no else, with 'Not'
--
-- > if !(Foe Here)
-- >   then Drop
-- > Mark 1
--
onlyIfNot = testCode expected input
  where expected = [Sense Here 2 1 Foe, Drop 2, Mark 1 3]
        input = compile $ StmBlock [ifte, MarkCall (ConstInt 1)]
        ifte = IfThenElse (Not (Condition Foe Here)) bThen bElse
        bThen = StmBlock [DropCall]
        bElse = StmBlock []

-- | If with two conditions in 'And'.
--
-- > if (Foe Here && Home Ahead )
-- >    then Drop
-- >    else Mark 1
--
andCond = testCode expected input
  where expected = [Sense Here 1 3 Foe, Sense Ahead 2 3 Home, Drop 4, Mark 1 4]
        input = compile $ IfThenElse cond bThen bElse
        cond  = And (Condition Foe Here) (Condition Home Ahead)
        bThen = StmBlock [DropCall]
        bElse = StmBlock [MarkCall (ConstInt 1)]

-- | Only if, no else, with two conditions in 'And'.
--
-- > if (Foe Here && Home Ahead )
-- >    then Drop
-- > Mark 1
--
onlyIfAnd = testCode expected input
  where expected = [Sense Here 1 3 Foe, Sense Ahead 2 3 Home, Drop 3, Mark 1 4]
        input = compile $ StmBlock [IfThenElse cond bThen bElse, next]
        cond  = And (Condition Foe Here) (Condition Home Ahead)
        bThen = StmBlock [DropCall]
        bElse = StmBlock []
        next  = MarkCall (ConstInt 1)

-- | If with two conditions in 'Or'.
--
-- > if (Foe Here || Home Ahead )
-- >    then Drop
-- >    else Mark 1
--
orCond = testCode expected input
  where expected = [Sense Here 2 1 Foe, Sense Ahead 2 3 Home, Drop 4, Mark 1 4]
        input = compile $ IfThenElse cond bThen bElse
        cond  = Or (Condition Foe Here) (Condition Home Ahead)
        bThen = StmBlock [DropCall]
        bElse = StmBlock [MarkCall (ConstInt 1)]

-- | Only if, no else, with two conditions in 'And'.
--
-- > if (Foe Here || Home Ahead )
-- >    then Drop
-- > Mark 1
--
onlyIfOr = testCode expected input
  where expected = [Sense Here 2 1 Foe, Sense Ahead 2 3 Home, Drop 3, Mark 1 4]
        input = compile $ StmBlock [IfThenElse cond bThen bElse, next]
        cond  = Or (Condition Foe Here) (Condition Home Ahead)
        bThen = StmBlock [DropCall]
        bElse = StmBlock []
        next  = MarkCall (ConstInt 1)
