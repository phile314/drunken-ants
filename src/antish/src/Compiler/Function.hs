module Compiler.Function (
    catchFunNotInScope
  , compileFunCall ) where

import Ast
import Compiler.Compile
import Compiler.Error

-- | Handler for FunNotInScope error.
-- A recursive function might be compiled right now, thus the function is not yet in scope, but 
-- the recursive call has already been tracked. If this is the case a jump back to this point
-- is generated, otherwise the error is propagated.
catchFunNotInScope :: [Expr]  -- The arguments given (must be 0 for a recursive function)
                  -> CError  -- Only 'FunNotInScope' can be recovered
                  -> Compile CState [Instruction]
catchFunNotInScope args (FunNotInScope iden) = do
  s <- getRecursiveCall iden
  case s of 
   Nothing -> throwError (FunNotInScope iden)
   Just s0 -> do checkArgs iden 0 args 
                 generate $ [Flip 1 s0 s0]    -- Jump back to starting point

catchFunNotInScope _ e = throwError e


-- | Compiles a function call, given its definition.
-- A 'CError' is thrown if the function is called with the wrong number of parameters.
compileFunCall :: Identifier  -- The name of the function
               -> [Expr]      -- The arguments passed to the function
               -> (Recursive, Int, [Expr] -> Compile CState [Instruction]) -- The function definition
               -> Compile CState [Instruction]
compileFunCall iden args (Rec, nargs, body) = do
  checkArgs iden nargs args
  s <- getRecursiveCall iden
  case s of 
    Just s0 -> generate $ [Flip 1 s0 s0]    -- Jump back to starting point
    Nothing -> body args

compileFunCall iden args (NonRec, nargs, body) = checkArgs iden nargs args >> body args


-- | @'checkArgs iden nargs args'@, throws an error if the number of arguments expected by the function
-- (@nargs@) does not match the number of arguments given (@args@).
checkArgs :: Identifier -> Int -> [Expr] -> Compile CState ()
checkArgs iden nargs args = do
  let given = length args
      wrongNum = WrongNumberParameters iden given nargs
  if nargs /= given
    then throwError wrongNum 
    else return ()
