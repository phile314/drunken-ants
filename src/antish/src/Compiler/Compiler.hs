-- | This module defines the compilation of the constructs of the language, i.e. how to compile them 
-- to low-lelel ant assembly.

module Compiler.Compiler where

import Compiler.Class
import Ast
import Assembly

instance (Compilable a) => Compilable [a] where
  compile s xs = concatMap (uncurry compile) $ zip [s..] xs  -- WRONG!! foldr

instance (Compilable a) => Compilable (Maybe a) where
  compile s (Just x) = compile s x
  compile s Nothing  = []

instance Compilable Program where
  compile s (Program sb) = compile s sb

instance Compilable (StmBlock) where
  compile s (StmBlock xs) = compile s xs

instance Compilable (Statement) where
  compile s (IfThenElse expr b1 b2) = 
    case expr of 
      And e1 e2      -> undefined
      Or  e1 e2      -> undefined
      Not e1         -> undefined
      Condition c sd ->
          let (s1, cb1) = (s + 1, compile s b1)
              l1 = length cb1
              (s2, cb2) = (s + l1, compile s2 b2) in
                [Sense sd s1 s2 c] ++ cb1 ++ cb2
