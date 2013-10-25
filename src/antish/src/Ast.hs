{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This module contains the datatypes that define the Abstract Syntax Tree of the language 

module Ast (
    module Assembly 
  , module Ast  -- TODO remove -- developing
  ) where

import Assembly
import Data.Tree
import Data.Data

type Identifier = String

type Import = String

data Program = Program [Import] [Binding]
  deriving (Eq, Data, Typeable)

data StmBlock = StmBlock [Statement]
  deriving (Eq, Data, Typeable, Show)

data Statement =
    FunCall Identifier [Expr]
  | IfThenElse Expr StmBlock StmBlock
  | For (Maybe Identifier) [Expr] StmBlock
  | Try StmBlock StmBlock
  | Let [Binding] StmBlock
  | WithProb Double StmBlock StmBlock
  | MarkCall Expr
  | UnMarkCall Expr
  | TurnCall Expr
  | DropCall
  | PickUpCall
  | MoveCall
  -- only jumps to the same or a higher level of the AST are supported, jumps down into the children of a node are not.
  | Label String
  | JumpTo String
  deriving (Eq, Data, Typeable, Show)

data Binding =
    VarDecl Identifier Expr
  | FunDecl Recursive Identifier [Identifier] StmBlock
  deriving (Eq, Data, Typeable, Show)

data Recursive = Rec | NonRec
  deriving (Eq, Data, Typeable, Show)
    
data Expr
  = ConstBool Bool
  | And Expr Expr
  | Or  Expr Expr
  | Not Expr
  | Condition Cond SenseDir
  | ConstInt Int
  | CDir LeftOrRight
  | VarAccess Identifier
  deriving (Eq, Show, Data, Typeable)

-- | Encodes the type of an expression
data EType = EBool | EInt | EDir
  deriving (Show, Eq)

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
  toTree (DropCall)            = Node "DropCall" []
  toTree (TurnCall lr)         = Node ("TurnCall" ++ show lr) []
  toTree (MoveCall)            = Node "MoveCall" []
  toTree (Label lbl)           = Node ("Label " ++ lbl) []
  toTree (JumpTo lbl)          = Node ("JumpTo " ++ lbl) []

instance ToTree Expr where
  toTree (ConstInt i)   = Node ("ConstInt " ++ (show i)) []
  toTree (ConstBool i)   = Node ("ConstBool " ++ (show i)) []
  toTree (VarAccess id) = Node ("VarAccess " ++ id) []
  toTree (Not b1)    = Node "Not" [toTree b1]
  toTree (And b1 b2) = Node "And" [toTree b1, toTree b2]
  toTree (Or  b1 b2) = Node "Or"  [toTree b1, toTree b2]
  toTree s = Node (show s) []

showTree :: ToTree a => a -> String
showTree t = drawTree $ toTree t

-- Without the newtype, ghc complains
-- see also http://stackoverflow.com/questions/7198907/haskell-constraint-is-no-smaller-than-the-instance-head
newtype UseTree a = UseTree a
instance ToTree a => Show (UseTree a) where
  show (UseTree x) = showTree x

instance Show Program where
	show p = showTree p
