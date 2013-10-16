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
  | FunDecl Identifier [Identifier] StmBlock
  deriving Eq

data BoolExpr =
    And BoolExpr BoolExpr
  | Or  BoolExpr BoolExpr
  | Not BoolExpr
  | Condition Cond SenseDir
  deriving Eq

data Expr =
  ConstInt Integer
  | VarAccess Identifier
  deriving (Show, Eq)

class ToTree a where
  toTree :: a -> Tree String

instance ToTree Program where
  toTree (Program ss) = Node "Program" [toTree ss]

instance ToTree StmBlock where
  toTree (StmBlock stms) = Node "StmBlock" (map toTree stms)

instance ToTree (Maybe StmBlock) where
  toTree Nothing = Node "Nothing" []
  toTree (Just sb) = Node "Some" [toTree sb]

instance ToTree Statement where
  toTree (FunCall id exs) = Node ("FunCall " ++ id) (map toTree exs)
  toTree (IfThenElse c s1 s2) = Node "IfThenElse" [toTree c, toTree s1, toTree s2]

instance ToTree Expr where
  toTree (ConstInt i)   = Node ("ConstInt " ++ (show i)) []
  toTree (VarAccess id) = Node ("VarAccess " ++ id) []

instance ToTree BoolExpr where
  toTree (And b1 b2) = Node "And" [toTree b1, toTree b2]
  toTree (Or  b1 b2) = Node "Or"  [toTree b1, toTree b2]

--instance ToTree a => Show a where
--  show t = drawTree $ toTree t

-- pretty printing program
{-
 - instance Show Program where
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
showBind n (FunDecl i args ss) = show i ++ show args ++ " {\n" ++ showBlock (n+1) ss ++ indent n ++"}"

indent :: Int -> String
indent k = concat ["  " | r <- [0..k]]

--}
