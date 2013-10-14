module Assembly (
    Instruction (..)
  , Cond (..)
  , SenseDir (..)
  , MarkerNumber
  , LeftOrRight (..)
  , AntState
  , AssemblyFrag
  ) where

-- FIX copy and paste:  it would be nice to import this directly from the Simulator mode 
type AntState = Int 

type AssemblyFrag = [Instruction]

data Instruction 
   = Sense SenseDir AntState AntState Cond
   | Mark MarkerNumber AntState
   | Unmark MarkerNumber AntState
   | PickUp AntState AntState
   | Drop AntState
   | Turn LeftOrRight AntState
   | Move AntState AntState
   | Flip Int AntState AntState
 deriving Show

data SenseDir = Here | Ahead | LeftAhead | RightAhead deriving (Show,Eq)

type MarkerNumber = Int -- 0..5

data LeftOrRight = IsLeft | IsRight deriving Show

data Cond = Friend | Foe | FriendWithFood | FoeWithFood 
          | Food | Rock | Marker Integer | FoeMarker | Home | FoeHome
            deriving (Show, Eq)
