-- | This module tests the scoping and the shadowing features.

module ScopeTest where

import Test.HUnit
import Ast
import Compiler.Core
import Compiler.Compile
import Util

-- | The tests that will be run
scopeTests :: Test
scopeTests = TestLabel "Scope" $ TestList tests
  where tests = [simpleDecl, shadowDecl, moreDecls, simpleFun, nestedFun, funCallInBlock, nestedCallInBlock]

-- | A simple expression declaration and use
--
-- > let x = Left in turn x
--
simpleDecl = testCode expected input
  where expected = [Turn IsLeft 1]
        input    = compile $ Let [varDecl] (StmBlock [TurnCall (VarAccess "x")])
        varDecl  = VarDecl "x" (CDir IsLeft)

-- | One level nesting, with shadowing
--
-- > let x = Left in
-- >    let x = Right in
-- >      turn x
--
shadowDecl = testCode expected input
  where expected  = [Turn IsRight 1]
        input     = compile $ (varDecl IsLeft) (StmBlock [varDecl IsRight b])
        varDecl e = Let [VarDecl "x" (CDir e)]
        b         = StmBlock [TurnCall (VarAccess "x")]

-- | Tests accessing variables at different levels
--
-- > let x = Left in
-- >    let y = Right in
-- >      WithProb 0.5 {Turn x} {Turn y}
--
moreDecls = testCode expected input
  where expected = [Flip 2 1 2, Turn IsLeft 3, Turn IsRight 3]
        input = compile $ varDecl IsLeft "x" (StmBlock [varDecl IsRight "y" b0])
        varDecl e iden = Let [VarDecl iden (CDir e)]
        b0 = StmBlock [WithProb 0.5 b1 b2]
        b1 = StmBlock [TurnCall (VarAccess "x")]
        b2 = StmBlock [TurnCall (VarAccess "y")]

-- | Simple function use
--
-- > let f x = Turn x in
-- >    f IsRight
--
simpleFun = testCode expected input
  where expected = [Turn IsRight 1]
        input    = compile $ Let [funDecl] (StmBlock [FunCall "f" [CDir IsRight]])
        funDecl  = FunDecl NonRec "f" ["x"] (StmBlock [TurnCall (VarAccess "x")])

-- | Tests function call inside a function
-- 
-- > let f x = Turn x in
-- >    let g x = f x in
-- >        f IsRight
--
nestedFun = testCode expected input
  where expected = [Turn IsRight 1]
        input    = compile $ funDecl "f" fBody b1
        fBody    = StmBlock [TurnCall (VarAccess "x")]
        gBody    = StmBlock [FunCall "g" [VarAccess "x"]]
        b1       = StmBlock [funDecl "g" gBody b2]
        b2       = StmBlock [FunCall "f" [CDir IsRight]]
        funDecl i b = Let [FunDecl NonRec i ["x"] b]

-- | Tests a function call inside a block NOT as first statement
--
-- > let f x = {Mark 1, Turn x} in
-- >    Drop
-- >    f Left
--
funCallInBlock = testCode expected input
  where expected = [Drop 1, Mark 1 2, Turn IsLeft 3]
        input = compile $ Let [f] (StmBlock [DropCall, FunCall "f" [(CDir IsLeft)]])
        f     = FunDecl NonRec "f" ["x"] (StmBlock [MarkCall (ConstInt 1), TurnCall (VarAccess "x")])

-- | Tested funcall inside another function call with other instructions around
--
-- > let g = {Drop; f Left, Mark 2}
-- > let f x = {Mark 1, Turn x} in
-- >    g 
--
nestedCallInBlock = testCode expected input
  where expected = [Drop 1, Mark 1 2, Turn IsLeft 3, Mark 2 4]
        input    = compile $ Let [g, f] (StmBlock [FunCall "g" []])
        g        = FunDecl NonRec "g" [] (StmBlock [DropCall, FunCall "f" [CDir IsLeft], MarkCall (ConstInt 2)])
        f        = FunDecl NonRec "f" ["x"] (StmBlock [MarkCall (ConstInt 1), TurnCall (VarAccess "x")])
