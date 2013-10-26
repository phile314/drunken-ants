-- | Defines tests cases for expected compilation failures with 'CError'.

module ErrorTest where

import Test.HUnit
import Ast
import Compiler.Core
import Compiler.Compile
import Compiler.Error
import Util
import RecursiveTest

-- | The tests that will be run
errorTests :: Test
errorTests = TestLabel "CError" $ TestList tests
  where tests = [notInScope, wrongNumberParameters, 
                 invalidMarker, wrongType, wrongTypeVar, 
                 notTailRec, invalidRecDecl]

-- | Tests that the proper CError is returned when a non-scoped function is used
notInScope :: Test
notInScope = testError expected input 
  where foo = "foo"
        expected = FunNotInScope foo
        input = compile $ FunCall foo []

-- | Tests that the proper CError is returned when the wrong number of parameters is used
wrongNumberParameters :: Test
wrongNumberParameters = testError expected input
  where expected = WrongNumParam foo pActual pExpected
        input = (addFunDecl foo pExpected NonRec (\_ -> return [])) >> (compile $ FunCall foo [undefined])
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

-- | Tests that the proper 'CError' is returned when a wrong type is used
wrongType :: Test
wrongType = testError expected input
  where expected = WrongType expr t1 t2
        (expr, t1, t2) = (ConstInt 1, EInt, EBool)
        input    = compile $ IfThenElse expr undefined undefined

-- | Tests that the proper 'CError' is returned when a variable of wrong type is used
wrongTypeVar :: Test
wrongTypeVar = testError expected input
  where expected = WrongType var t1 t2
        (var, t1, t2) = (VarAccess "x", EInt, EBool)
        input = compile $ varDecl (StmBlock [IfThenElse var undefined undefined])
        varDecl = Let [VarDecl "x" (ConstInt 1)]

-- | Runs the 'nonTailRecursive' test for each statement of the Ast.
notTailRec :: Test
notTailRec = TestLabel "TailRec" $ TestList [notTailRecursive st | st <- statements]
  where s = StmBlock [DropCall]
        statements = [IfThenElse (Condition Foe Ahead) s s, Try s s,
                      For Nothing [] s, Let [] s, 
                      WithProb 1 s s]
                      
-- | Tests that 'NonTailRecursive' error is raised when compiling
-- a non tail recursive function.
notTailRecursive :: Statement -> Test
notTailRecursive s = testError expected input
  where expected = NotTailRecursive "f" body
        body     = StmBlock [s]
        input    = compile $ Let [f] (StmBlock [recCall "f"])
        f        = FunDecl Rec "f" [] (StmBlock [s])

-- | Tests that 'InvalidRecFun' is raised when a function with non-zero
-- arguments is declared.
invalidRecDecl :: Test
invalidRecDecl = testError expected input
  where expected = InvalidRecFun "f" args
        args     = ["x1", "x2"]
        input    = compile $ Let [f] undefined
        f        = FunDecl Rec "f" args undefined
