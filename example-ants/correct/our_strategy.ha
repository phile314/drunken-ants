main = {
    with probability 1.0 do {
        randDir;
        explore;
        nothing;
    } otherwise {
        nothing;
    }
}

nop = { Turn Left; Turn Right; }

rec nothing = {
    Drop;
    nothing;
}

checkField = {
    for [1,2,3,4,5] {
        if (Marker 1 Ahead || Marker 2 Ahead || Marker 3 Ahead) then {
            Turn Right;
            Move;
            Move;
            explore;
        }
    }
}

rec explore = {
    let f k1 k2 = {
        checkField;
--        Mark 5;
          Move;
--        checkField;
        Mark k1;
        isThereFood;
 --       Turn Right;
 --       Turn Right;
--        Move;
        checkField;
        Mark k2;
        isThereFood;
        Move;
--        Turn Left;
--        Turn Left;
    },
    g m = {
        isThereFood;
        checkField;
        Move;
        Mark m;
        
    }
     in {
        try {
            g 3;
            g 2;
            g 1;
            explore;
        } catch {
            with probability 0.5 do {
                Turn Left;
            } otherwise {
                Turn Right;
            }
            explore;
        }
    }
}

isThereFood = {
    if Food Here then {
        PickUp;
        turnAround;
        goHome;
--        explore;
    }
}

turnAround = { for [1,1,1] { Turn Left; } }

rec f1 = {
    if Marker 2 Ahead then {
        reallyMove;
    } else {
        if Marker 3 Ahead then {
            turnAround;
        } else {
            if Marker 2 LeftAhead then {
                Turn Left;
                reallyMove;
            } else {
                if Marker 2 RightAhead then {
                    Turn Right;
                    reallyMove;
                } else {
                    Turn Left;
                    goHome;
                }
            }
        }
    }
    f2;
}
rec f2 = {
    if Marker 3 Ahead then {
        reallyMove;
    } else {
        if Marker 1 Ahead then {
            turnAround;
        } else {
            if Marker 3 LeftAhead then {
                Turn Left;
                reallyMove;
            } else {
                if Marker 3 RightAhead then {
                    Turn Right;
                    reallyMove;
                } else {
                    Turn Left;
                    goHome;
                }
            }
        }
    }
    f3;
}
rec f3 = {
    if Marker 1 Ahead then {
        reallyMove;
    } else {
        if Marker 2 Ahead then {
            turnAround;
        } else {
            if Marker 1 LeftAhead then {
                Turn Left;
                reallyMove;
            } else {
                if Marker 1 RightAhead then {
                    Turn Right;
                    reallyMove;
                } else {
                    Turn Left;
                    goHome;
                }
            }
        }
    }
    f1;
}
-- I suppose I am bringing food.
-- It will bring the food home following the path (1,2,3)
-- and eventually when there will call protectFood
rec goHome = {
    if Home Here then {
      Drop;
      protectFood;
    }
    if Marker 1 Ahead then {
        f3;
    }
    if Marker 2 Ahead then {
        f1;
    }
    if Marker 3 Ahead then {
        f2;
    }
--    with probability 0.5 do {
--        Turn Left;
--    } otherwise {
--        Turn Right;
--    }
--    with probability 0.1 do {
--        Move;
--    } otherwise { nop; }
    goHome;
}

rec reallyMove = {
    try {
        Move;
    } catch {
        reallyMove;
    }
}

randDir = {
    with probability 0.5 do {
        for [1,2,3] {
            Turn Left;
        }
    } otherwise { Drop; }
    with probability 0.33 do {
        Turn Left;
    } otherwise {
        with probability 0.5 do {
            Turn Right;
        } otherwise { nop; }
    }
}

mark o i = {
    Mark o;
    Move;
    isThereFood;
    Mark i;
    Move;
    isThereFood;
    Mark o;
}

-- After bringing back some food an ant stops on the first anthill cell it founds,
-- drops the food, and waits until another ant with other food comes back.
-- The new ant will substitute the first ant in the task of protecting the food.
-- The first ant starts exploring again.
rec protectFood = {
  -- I suppose I am on an anthill cell and I have already dropped my food
  if FriendWithFood Ahead then {
    explore;
  } else {
    Turn Left;
    protectFood;
  }
}

