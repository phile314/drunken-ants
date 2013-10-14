-- | Defines the compiling operations for the elements of the Ast

module Compiler.Class where

import Assembly (Instruction, AntState) 
import Compiler.Compile
import Compiler.CompileT 
import Ast
import Control.Monad.State

class Compilable c where
   compile :: c -> Compile (CState AntState) [Instruction]

instance (Compilable a) => Compilable (Maybe a) where
  compile (Just x) = compile x
  compile Nothing = undefined

instance Compilable StmBlock where
  compile = undefined

-- this is not compiling at the moment
{--instance Compilable Statement where
  compile (IfThenElse expr b1 b2) = do
    case expr of 
      And e1 e2      -> undefined
      Or  e1 e2      -> undefined
      Not e1         -> undefined
      Condition c sd -> do
        let c1 = compile b1 
            c2 i1 = do 
               modify (+1)
               i2 <- compile b2
               modify (+length i2)
               return (i1 ++ i2)
        s0 <- get
        let s1 = s0 + 1
        i1 <- c1
        i2 <- (c2 i1)
        let s2 = s1 + length i1
        modify $ \x -> x + 1 + length i12
        return ([Sense sd s1 s2 c] ++ i1 ++ i2) -- missing jumps

--}
