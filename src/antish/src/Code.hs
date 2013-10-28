module Code where

import Assembly

class Code c where
  toCode :: (Show c) => c -> String

instance Code Cond where
  toCode (Marker n) = "Marker " ++ show n
  toCode x          = show x

instance (Code c, Show c) => Code [c] where
  toCode xs = unlines $ map toCode xs

instance Code Instruction where
  toCode (Sense sd s0 s1 c) = unwords $ ["Sense", show sd, show s0, show s1, toCode c]
  toCode i = show i
