module Simplify
  ( simplify
  , module Simplify.Types
  , module Simplify.Inline
  , module Simplify.Reduce
  , module Simplify.PropConsts
  , module Simplify.MkLiterals
  , test1, test2)
where

import Control.Monad
import Control.Monad.Identity
import Ast
import Simplify.Reduce
import Simplify.Inline
import Simplify.Types
import Simplify.PropConsts
import Simplify.InlineT
import Simplify.MkLiterals

-- | Produces a simpler version of a program. The result
--   is semantically equivalent to the original version.
simplify :: Program -> Either TError Program
simplify p = do
  let p' = runIdentity $ transf mkLiterals p
  p'' <- transf inline p'
  let p''' = runIdentity $ transf propConsts p''
  return p'''



test1 = Program [] $ [VarDecl "x" (ConstBool True), FunDecl Rec "main" [] (StmBlock [IfThenElse (And (ConstBool True) (VarAccess "x")) (StmBlock [DropCall]) (StmBlock [MoveCall])])]
test2 = Program [] $ [FunDecl Rec "main" [] (StmBlock [For (Just "x") [(ConstBool True), (ConstBool False)] (StmBlock [IfThenElse (And (ConstBool True) (VarAccess "x")) (StmBlock [DropCall]) (StmBlock [MoveCall])])])]
