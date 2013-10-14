-- | This module contains the datatypes that define the Abstract Syntax Tree of the language 

module Ast (
    module Assembly 
  , module Ast  -- TODO remove -- developing
  ) where

import Assembly

type Identifier = String

data Program = Program StmBlock
  deriving Eq

data StmBlock = StmBlock [Statement]
  deriving Eq

data Statement =
    FunCall Identifier [Expr]
  | IfThenElse BoolExpr StmBlock (Maybe StmBlock)
  | For (Maybe Identifier) [Expr] StmBlock
  | Try StmBlock StmBlock StmBlock
  | Let [Binding] StmBlock
  | WithProb Double StmBlock StmBlock
  deriving Eq

data Binding =
    VarDecl Identifier Expr
  | FunDecl Identifier StmBlock
  deriving Eq

data BoolExpr =
    And BoolExpr BoolExpr
  | Or  BoolExpr BoolExpr
  | Not BoolExpr
  | Condition Cond SenseDir
  deriving Eq

data Expr =
  ConstInt Integer
--  | FunCall FunIdentifier [Expr]
  | VarAccess Identifier
  deriving (Show, Eq)

-- pretty printing program
instance Show Program where
	show (Program s) = '\n' : show s
instance Show StmBlock where
	show = showBlock 0 
instance Show BoolExpr where
	show (And x y)			= "(" ++ show x ++ " And " ++ show y ++ ")"
	show (Or x y)			= "(" ++ show x ++ " Or " ++ show y ++ ")"
	show (Not x)			= "!(" ++ show x ++")"
	show (Condition c s)	= "(Condition " ++ show c ++ " " ++ show s ++ ")"

showBlock :: Int -> StmBlock -> String
showBlock n (StmBlock ss) = unlines $ map (showStatement n) ss

showStatement :: Int -> Statement -> String
showStatement n s = indent n ++ showStm s
	where
		showStm (FunCall i [])				= "" ++ i ++ "()"
		showStm (FunCall i es)				= "" ++ i ++ show es 
		showStm (IfThenElse b xs (Just ys))	= "if " ++ show b ++ " then {\n" ++ showBlock (n+1) xs ++ indent n ++"} else {\n" ++ showBlock (n+1) ys ++ indent n ++"}"
		showStm (IfThenElse b xs Nothing)	= "if " ++ show b ++ " then {\n" ++ showBlock (n+1) xs ++ indent n ++"} else { }"
		showStm (For (Just i) es ss)		= "for ("++ show i ++")" ++ show es ++ "{\n" ++ showBlock (n+1) ss ++ indent n ++ "}"
		showStm (For Nothing es ss)			= "for " ++ show es ++ "{\n" ++ showBlock (n+1) ss ++ indent n ++"}"
		showStm (Try t ss c)				= "try {\n" ++ showBlock (n+1) t ++ indent n ++"} with {\n" ++ showBlock (n+1) ss ++ indent n ++"} catch {\n" ++ showBlock (n+1) c ++ indent n ++"}"
		showStm (Let [b] ss)  				= "let " ++ showBind n b ++ "in" ++ showBlock (n+1) ss
		showStm (Let bs ss) 				= "let " ++ foldr (\b s -> '\n' : indent (n+1) ++ showBind (n+1) b ++ s) "" bs ++ indent n ++ "in" ++ showBlock (n+1) ss
		showStm (WithProb d w ss)			= "with propability "++ show d ++" do {\n"++ showBlock (n+1) w ++ indent n ++"} otherwise {\n" ++ showBlock (n+1) ss ++ indent n ++"}"

showBind :: Int -> Binding -> String
showBind n (VarDecl i e) = show i ++ " = " ++ show e
showBind n (FunDecl i ss) = show i ++ " {\n" ++ showBlock (n+1) ss ++ indent n ++"}"

indent :: Int -> String
indent k = concat ["  " | r <- [0..k]]
