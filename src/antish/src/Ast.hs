-- | This module contains the datatypes that define the Abstract Syntax Tree of the language 

module Ast where

type Identifier = String

data Program = Program StmBlock
  deriving (Show, Eq)

data StmBlock = StmBlock [Statement]
  deriving (Show, Eq)

data Statement =
    FunCall FIdent [Expr]
  | IfThenElse BoolExpr StmBlock (Maybe StmBlock)
  | For (Maybe Identifier) [Expr] StmBlock
  | Try StmBlock StmBlock StmBlock
  | Let [Binding] StmBlock
  | WithProb Double StmBlock StmBlock
  deriving (Show, Eq)

data Binding =
    VarDecl Identifier Expr
  | FunDecl FIdent [Identifier] StmBlock
  deriving (Show, Eq)

data FIdent = FIdent Identifier -- Should be dropped for Var = (Identifier, Type)
  deriving (Show, Eq)

data BoolExpr =
    And BoolExpr BoolExpr
  | Or  BoolExpr BoolExpr
  | Not BoolExpr
  | Condition Cond SenseDir
  deriving (Show, Eq)

data Expr =
  ConstInt Integer
--  | FunCall FunIdentifier [Expr]
  | VarAccess Identifier
  deriving (Show, Eq)

data SenseDir = Here | Ahead | LeftAhead | RightAhead
  deriving (Show, Eq)

data Cond = Friend | Foe | FriendWithFood | FoeWithFood 
          | Food | Rock | Marker Integer | FoeMarker | Home | FoeHome
            deriving (Show, Eq)
