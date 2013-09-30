-- | This module contains the datatypes that define the Abstract Syntax Tree of the language 

module Ast where

type Identifier = String


data Program = Program StmBlock

data StmBlock = [Statement]

data Statement =
    FunCall FunIdentifier [Expr]
  | IfThenElse Expr StmBlock StmBlock
  | For (Maybe Identifier) Expr StmBlock
  | Try StmBlock StmBlock StmBlock
  | Let [Binding] StmBlock
  | WithProb Double StmBlock StmBlock

data Binding =
    VarDecl Identifier Expr
  | FunDecl Identifier [Identifier] StmBlock

data FunIdentifier =
    VIdent Identifier
  | CIdent Identifier

data BoolExpr =
    And Expr Expr
  | Or  Expr Expr
  | Not Expr

data Expr =
    BoolExpr
  | IntExpr
--  | ListExpr [Expr]
--  | FunCall FunIdentifier [Expr]
  | Prec Expr


data IntExpr =
    ConstInt Integer
  | VarAccess Identifier
