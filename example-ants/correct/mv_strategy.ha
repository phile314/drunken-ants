rec main = {
  explore;
}

turnBack = { 
  for [0,1,2] {
    Turn Left;
  }
}

rec goHome = { Move; goHome; }

rec getFoodGoHome = {
  try { 
    PickUp;
    turnBack;
    goHome;
  } catch {
    explore;
  }
}

-- | Moves to a clear cell.
rec moveClear = {
  let marks = (Marker 1 Ahead || Marker 2 Ahead || Marker 3 Ahead),
      obstacles = (Rock Ahead || Foe Ahead || Friend Ahead),
      clear = (marks || obstacles) in {
--        for [0,1,2,3,4] {
          if !( clear )
            then { Turn Left; }
            else { Move; explore; }
        moveClear;                   -- retry hoping to get unstuck
--      }
  }
}
    

-- | Explores in straight lines the ground, marking its path sequentially with 3, 2, 1.
rec explore = {
--  for x in [3, 2, 1] {
    getFoodGoHome;
--    Mark x;
    moveClear;
--  }
}

{-
-- | Spreads the ant in different direction
spread = {
  with probability 0.5 do { randomDir; Move; }
  otherwise { Move; }
}

-- Turn the ant in a random direction
randomDir = {
  with probability 0.5 do { Turn Left; }
  otherwise { Turn Right; }
}
-}

-- Marks used
    -- 1 2 3 points to anthill
    -- 3 2 1 ponts to food
