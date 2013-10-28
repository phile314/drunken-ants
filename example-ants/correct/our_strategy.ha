main = {
    randDir;
    explore;
--    goHome;
--    main;
    nothing;
}

nop = { Turn Left; Turn Right; }

rec nothing = {
    Drop;
    nothing;
}


rec explore = {
    let f td1 td2 = {
        Turn td1;
        mark 5 3;
        Turn td1;
        Move;
        isThereFood;
        Turn td1;
        mark 5 2;
        Turn td2;
        Move;
        isThereFood;
        Turn td2;
        mark 5 1;
        Turn td1;
        Move;
        isThereFood;
    } in {
        try {
            f Left Right;
            f Right Left;
            explore;
        } catch {
            explore;
        }
    }
}

isThereFood = {
    if Food Here then {
        PickUp;
        goHome;
        explore;
    }
}

turnAround = { for [1,1,1] { Turn Left; } }

rec f1 = {
    if Marker 2 Ahead then {
        Move;
    } else {
        if Marker 3 Ahead then {
            turnAround;
        } else {
            if Marker 2 LeftAhead then {
                Turn Left;
                Move;
            } else {
                if Marker 2 RightAhead then {
                    Turn Right;
                    Move;
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
        Move;
    } else {
        if Marker 1 Ahead then {
            turnAround;
        } else {
            if Marker 3 LeftAhead then {
                Turn Left;
                Move;
            } else {
                if Marker 3 RightAhead then {
                    Turn Right;
                    Move;
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
        Move;
    } else {
        if Marker 2 Ahead then {
            turnAround;
        } else {
            if Marker 1 LeftAhead then {
                Turn Left;
                Move;
            } else {
                if Marker 1 RightAhead then {
                    Turn Right;
                    Move;
                } else {
                    Turn Left;
                    goHome;
                }
            }
        }
    }
    f1;
}
rec goHome = {
    if Marker 1 Ahead then {
        f1;
    }
    if Marker 2 Ahead then {
        f2;
    }
    if Marker 3 Ahead then {
        f3;
    }
    with probability 0.5 do {
        Turn Left;
    } otherwise {
        Turn Right;
    }
    with probability 0.1 do {
        Move;
    } otherwise { nop; }
    goHome;
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
