-- | This module contains the datatypes that define the Abstract Syntax Tree of the language 

module Ast where

type Identifier = String

data Program = Program StmBlock
  deriving (Show, Eq)

data StmBlock = StmBlock [Statement]
  deriving (Show, Eq)

data Statement =
    FunCall FunIdentifier [Expr]
  | IfThenElse BoolExpr StmBlock StmBlock
  | For (Maybe Identifier) Expr StmBlock
  | Try StmBlock StmBlock StmBlock
  | Let [Binding] StmBlock
  | WithProb Double StmBlock StmBlock
  deriving (Show, Eq)

data Binding =
    VarDecl Identifier Expr
  | FunDecl Identifier [Identifier] StmBlock
  deriving (Show, Eq)

data FunIdentifier =
    VIdent Identifier
  | CIdent Identifier
  deriving (Show, Eq)

data BoolExpr =
    And BoolExpr BoolExpr
  | Or  BoolExpr BoolExpr
  | Not BoolExpr
  deriving (Show, Eq)

data Expr =
  ConstInt Integer
--  | ListExpr [Expr]
--  | FunCall FunIdentifier [Expr]
  | VarAccess Identifier
  deriving (Show, Eq)
