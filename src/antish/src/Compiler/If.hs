-- | This module is concerned with the compilation of if-then-else statements

{-# LANGUAGE FlexibleContexts #-}

module Compiler.If (compileIf) where

import Ast
import Compiler.Utility
import Compiler.Class
import Compiler.Compile

-- | Compiles an arbitrary IfThenElse statement.
-- The expression involved must be already reduced to basic values.
compileIf :: (Compilable Statement, Jumpable StmBlock) => 
             Expr       -- ^ A reduced Boolean expression
          -> StmBlock   -- ^ The "then" branch
          -> StmBlock   -- ^ The "else" branch
          -> Compile CState [Instruction]
compileIf (And e1 e2) b1 b2 = do
  (Sense c1 s1 _ sd1):xs <- compile (IfThenElse e1 (StmBlock []) (StmBlock []))
  if2@((Sense _ s3 s4 _):_) <- compileIf e2 b1 b2
  return $ [Sense c1 s1 s4 sd1] ++ xs ++ if2

compileIf (Or e1 e2) b1 b2 = do
  (Sense c1 s0 _ sd1):xs <- compileIf e1 (StmBlock []) (StmBlock [])
  if2@((Sense _ s1 s2 _):_) <- compileIf e2 b1 b2
  return $ [Sense c1 s1 s0 sd1] ++ xs ++ if2

compileIf (Not e) b1 b2 = do
  (Sense c s1 s2 sd):xs <- compileIf e b1 b2
  return $ (Sense c s2 s1 sd):xs

compileIf (Condition c sd) b1 b2 = compileAndReorder ifte b1 b2
  where ifte s1 s2 = Sense sd s1 s2 c
