-- | This module defines 'Scope' the data structure used to maintain nested scopes

module Compiler.Scope where

import qualified Data.Map as Map
import Control.Monad

-- | This data structure provides nested scoping.
-- Upon a lookup the first level is used, if it fails the second outer level 
-- is used and so on, until either a match is found or all the levels all 
-- searched.
type Scope k v = [Map.Map k v]

-- | Represents an empty 'Scope' map.
empty :: Scope k v
empty = [Map.empty]

-- | Insert a key value entry in the current level.
insert :: Ord k => k -> v -> Scope k v -> Scope k v
insert key val (x:xs) = (Map.insert key val x):xs

-- | The lookup is performed first in the current level and in case of 
-- failures to the upper level. If all the levels are consumed without 
-- any match Nothing is returned.
lookup :: Ord k => k -> Scope k v -> Maybe v
lookup key = foldl lookup' Nothing
  where lookup' prev nextScope = prev `mplus` (Map.lookup key nextScope)

-- | @'father' s1 s2@ makes the levels of the 'Scope' map @s1@ the upper levels of @s2@
father :: Scope k v -> Scope k v -> Scope k v
father s1 [] = s1
father s1 (x:xs) = (x:s1) ++ xs

-- | @'children' s1 s2@ makes the levels of the 'Scope' map @s1@ the most inner levels before @s2@
children :: Scope k v -> Scope k v -> Scope k v
children s1 [] = s1
children s1 s2 = s1 ++ s2

-- | Removes the current level, it fails with error if there is no current level
remove :: Scope k v -> Scope k v
remove (x:xs) = xs
remove []     = error "No current level" 
