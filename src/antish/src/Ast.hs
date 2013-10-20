{-# LANGUAGE FlexibleInstances #-}
-- | This module contains the datatypes that define the Abstract Syntax Tree of the language 

module Ast (
    module Assembly 
  , module Ast  -- TODO remove -- developing
  ) where

import Assembly
import Data.Tree

type Identifier = String

data Program = Program StmBlock
  deriving Eq

data StmBlock = StmBlock [Statement]
  deriving (Eq, Show)

data Statement =
    FunCall Identifier [Expr]
  | IfThenElse BoolExpr StmBlock (Maybe StmBlock)
  | For (Maybe Identifier) [Expr] StmBlock
  | Try StmBlock StmBlock
  | Let [Binding] StmBlock
  | WithProb Double StmBlock StmBlock
  | MarkCall Int              -- TODO replace with Expr
  | UnMarkCall Int            -- TODO replace with Expr
  | TurnCall LeftOrRight      -- TODO replace with Expr
  | DropCall
  | PickUpCall
  | MoveCall
  deriving (Eq, Show)

data Binding =
    VarDecl Identifier Expr
  | FunDecl Identifier [Identifier] StmBlock
  deriving (Eq, Show)

data BoolExpr =
    And BoolExpr BoolExpr
  | Or  BoolExpr BoolExpr
  | Not BoolExpr
  | Condition Cond SenseDir
  deriving (Eq, Show)

data Expr =
  ConstInt Integer
  | VarAccess Identifier
  deriving (Eq, Show)

class ToTree a where
  toTree :: a -> Tree String

instance ToTree Program where
  toTree (Program ss) = Node "Program" [toTree ss]

instance ToTree StmBlock where
  toTree (StmBlock stms) = Node "StmBlock" (map toTree stms)

instance ToTree a => ToTree (Maybe a) where
  toTree Nothing   = Node "Nothing" []
  toTree (Just x) = Node "Just" [toTree x]

instance ToTree a => ToTree [a] where
  toTree ts = Node "List" (map toTree ts)

instance ToTree Binding where
  toTree (VarDecl id ex)     = Node "VarDecl" [(Node id []), toTree ex]
  toTree (FunDecl id ids ss) = Node "FunDecl" ((map (\s -> Node s []) (id:ids)) ++ [toTree ss])

instance ToTree Statement where
  toTree (FunCall id exs)      = Node ("FunCall " ++ id) (map toTree exs)
  toTree (IfThenElse c s1 s2)  = Node "IfThenElse" [toTree c, toTree s1, toTree s2]
  toTree (For id es ss)        = Node "For" [(Node (show id) []), toTree es, toTree ss]
  toTree (Try s1 s2)        = Node "Try" [toTree s1, toTree s2]
  toTree (Let bs ss)           = Node "Let" [toTree bs, toTree ss]
  toTree (WithProb p s1 s2)    = Node "WithProb" [(Node (show p) []), toTree s1, toTree s2]

instance ToTree Expr where
  toTree (ConstInt i)   = Node ("ConstInt " ++ (show i)) []
  toTree (VarAccess id) = Node ("VarAccess " ++ id) []

instance ToTree BoolExpr where
  toTree (Not b1)    = Node "Not" [toTree b1]
  toTree (And b1 b2) = Node "And" [toTree b1, toTree b2]
  toTree (Or  b1 b2) = Node "Or"  [toTree b1, toTree b2]
  toTree (Condition c d) = Node ("Condition " ++ show c ++ " " ++ show d) []

showTree :: ToTree a => a -> String
showTree t = drawTree $ toTree t

-- Without the newtype, ghc complains
-- see also http://stackoverflow.com/questions/7198907/haskell-constraint-is-no-smaller-than-the-instance-head
newtype UseTree a = UseTree a
instance ToTree a => Show (UseTree a) where
  show (UseTree x) = showTree x

instance Show Program where
	show p = showTree p
