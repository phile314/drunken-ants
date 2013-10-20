-- | This module tests the 'Try' construct compilation
module TryTest where

import Test.HUnit
import Ast
import Compiler.Class
import Compiler.Compile
import Util

-- | The tests that will be run
tryTests :: Test
tryTests = TestLabel "Try" $ TestList [simpleTry, complexTry]

-- | Tests a simple try
--
-- > try PickUp 
-- >  catch Drop
-- > Mark 1
--
simpleTry = testCode expected input
  where expected = [PickUp 2 1, Drop 2, Mark 1 3]
        input = compile $ StmBlock [try, next]
        try = Try (StmBlock [PickUpCall]) (StmBlock [DropCall])
        next = MarkCall (ConstInt 1)

-- | Tests a more elaborated try
--
-- > try {
-- >    Drop
-- >    PickUp
-- >    UnMark 1
-- >    PickUp 1
-- > } catch {
-- >    Mark 1
-- >    Turn Left
-- > }{ Unmark 1
-- >    Turn Right }
--
complexTry = testCode expected input
  where expected = try ++ catch ++ next
        try = [Drop 1, PickUp 2 4, Unmark 1 3, PickUp 6 4]
        catch = [Mark 1 5, Turn IsLeft 6]
        next = [Unmark 1 7, Turn IsRight 8]
        input = compile $ StmBlock [Try b1 b2, UnMarkCall (ConstInt 1), TurnCall IsRight]
        b1 = StmBlock [DropCall, PickUpCall, UnMarkCall (ConstInt 1), PickUpCall]
        b2 = StmBlock [MarkCall (ConstInt 1), TurnCall IsLeft]
