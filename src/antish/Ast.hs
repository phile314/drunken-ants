--module Ants.Ast where
module Ast where



type Identifier = String


data Program = Program StmBlock
  deriving (Show)

data StmBlock = StmBlock [Statement]
  deriving (Show)

data Statement =
    Call Expr
  | IfThenElse Expr StmBlock StmBlock
  | For (Maybe Identifier) Expr StmBlock
  | Try StmBlock StmBlock StmBlock
  | Let [Binding] StmBlock
  | WithProb Double StmBlock StmBlock
  deriving (Show)

data Binding =
    VarDecl Identifier Expr
  | FunDecl Identifier [Identifier] StmBlock
  deriving (Show)

data FunIdentifier =
    VIdent Identifier
  | CIdent Identifier
  deriving (Show)

data BoolExpr =
    And Expr Expr
  | Or  Expr Expr
  | Not Expr
  deriving (Show)

data Expr =
    Appl Expr Expr
  | BoolExpr BoolExpr
  | ListLiteral [Expr]
--  | FunCall FunIdentifier [Expr]
  | ConstBool Bool
  | ConstInt Int
  | VarAccess Identifier
  | OpCall Expr Identifier Expr
  deriving (Show)

