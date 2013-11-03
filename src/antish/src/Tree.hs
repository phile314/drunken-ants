-- | This module defines the class 'ToTree' that allows to build 
-- a multi-way tree from the abstract syntax tree.
-- It is used mostly in order to provide a neat printing for debugging purposes.

module Tree where

import Ast
import Data.Tree

class ToTree a where
  toTree :: a -> Tree String

instance ToTree Program where
  toTree (Program im tl) = Node "Program" [toTree' im, toTree tl]
    where toTree' xs = Node "Import" $ map (\s -> Node s []) xs

instance ToTree a => ToTree (Maybe a) where
  toTree Nothing   = Node "Nothing" []
  toTree (Just x) = Node "Just" [toTree x]

instance ToTree a => ToTree [a] where
  toTree ts = Node "List" (map toTree ts)

instance ToTree Binding where
  toTree (VarDecl id ex)     = Node "VarDecl" [(Node id []), toTree ex]
  toTree (FunDecl rec id ids ss) = Node ("FunDecl" ++ show rec) forest
    where forest = ((map (\s -> Node s []) (id:ids)) ++ [toTree ss])

instance ToTree StmBlock where
  toTree (StmBlock stms)       = Node "StmBlock" (map toTree stms)

instance ToTree Statement where
  toTree (FunCall id exs)      = Node ("FunCall " ++ id) (map toTree exs)
  toTree (IfThenElse c s1 s2)  = Node "IfThenElse" [toTree c, toTree s1, toTree s2]
  toTree (For id es ss)        = Node "For" [(Node (show id) []), toTree es, toTree ss]
  toTree (Try s1 s2)           = Node "Try" [toTree s1, toTree s2]
  toTree (Let bs ss)           = Node "Let" [toTree bs, toTree ss]
  toTree (WithProb p s1 s2)    = Node "WithProb" [(Node (show p) []), toTree s1, toTree s2]
  toTree (PickUpCall)          = Node "PickUpCall" []
  toTree (DropCall)            = Node "DropCall" []
  toTree (TurnCall lr)         = Node ("TurnCall" ++ show lr) []
  toTree (MoveCall)            = Node "MoveCall" []
  toTree (MarkCall n)          = Node ("MarkCall" ++ show n) []

instance ToTree Expr where
  toTree (ConstInt i)   = Node ("ConstInt " ++ (show i)) []
  toTree (ConstBool i)   = Node ("ConstBool " ++ (show i)) []
  toTree (VarAccess id) = Node ("VarAccess " ++ id) []
  toTree (Not b1)    = Node "Not" [toTree b1]
  toTree (And b1 b2) = Node "And" [toTree b1, toTree b2]
  toTree (Or  b1 b2) = Node "Or"  [toTree b1, toTree b2]
  toTree s = Node (show s) []

-- | Provides a neat representation of a 'ToTree' value.
drawAst :: ToTree a => a -> String
drawAst = drawTree . toTree
