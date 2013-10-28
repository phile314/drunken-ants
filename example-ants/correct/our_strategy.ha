main = {
    randDir;
    explore;
--    goHome;
--    main;
    nothing;
}

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
        f2;
    } else {
        if Marker 3 Ahead then {
            turnAround;
            f2;
        }
        -- TODO else
    }
}
rec f2 = {
    if Marker 3 Ahead then {
        Move;
        f3;
    } else {
        if Marker 1 Ahead then {
            turnAround;
            f3;
        }
        -- TODO else
    }
}
rec f3 = {
    if Marker 1 Ahead then {
        Move;
        f1;
    } else {
        if Marker 2 Ahead then {
            turnAround;
            f1;
        }
        -- TODO else
    }
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
        } otherwise { Drop; }
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
