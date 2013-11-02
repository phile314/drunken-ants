{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module contains the datatypes that define the Abstract Syntax Tree of the language 

module Ast (
    module Assembly
  , module Ast ) where

import Assembly
import Data.Data

type Identifier = String

-- | The name of an imported module
type Import = String

data Program = Program [Import] [Binding]
  deriving (Eq, Data, Typeable, Show)

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
  deriving (Eq, Data, Typeable, Show)

-- | Declaration of a "variable" or a (local) function.
data Binding =
    VarDecl Identifier Expr
  | FunDecl Recursive Identifier [Identifier] StmBlock
  deriving (Eq, Data, Typeable, Show)

-- | Tags wether a function is recursive
data Recursive = Rec | NonRec
  deriving (Eq, Data, Typeable, Show)
   
-- | Encodes untyped expression
-- Their type is checked during the compiling phase, whenever they are used. 
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
