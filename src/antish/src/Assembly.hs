{-# LANGUAGE DeriveDataTypeable #-}

-- | This module, mainly copied from the evaluator, defines datatypes for the assembly language.

module Assembly (
    Instruction (..)
  , Cond (..)
  , SenseDir (..)
  , MarkerNumber
  , LeftOrRight (..)
  , AntState
  ) where

import Data.Data

type AntState = Int 

data Instruction 
   = Sense SenseDir AntState AntState Cond
   | Mark MarkerNumber AntState
   | Unmark MarkerNumber AntState
   | PickUp AntState AntState
   | Drop AntState
   | Turn LeftOrRight AntState
   | Move AntState AntState
   | Flip Int AntState AntState
 deriving (Show, Eq)

data SenseDir = Here | Ahead | LeftAhead | RightAhead deriving (Show, Eq, Data, Typeable)

type MarkerNumber = Int -- 0..5

data LeftOrRight = IsLeft | IsRight deriving (Eq, Data, Typeable)

instance Show LeftOrRight where
    show IsLeft  = "Left"
    show IsRight = "Right"

data Cond = Friend | Foe | FriendWithFood | FoeWithFood 
          | Food | Rock | Marker Integer | FoeMarker | Home | FoeHome
            deriving (Show, Eq, Data, Typeable)
