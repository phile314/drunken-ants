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
  | Let [Binding]
  | WithProb Double StmBlock StmBlock
  deriving (Show)

data Binding =
    VarDecl Identifier StmBlock
  | FunDecl Identifier [Identifier] StmBlock
  deriving (Show)

data Expr =
    Appl Expr Expr
  | ListLit [Expr]
  | BoolLit Bool
  | IntLit Int
  | VarAccess Identifier
  | OpCall Expr Identifier Expr

  | Lambda Identifier Expr
  deriving (Show)

