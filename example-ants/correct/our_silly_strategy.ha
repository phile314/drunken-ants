rec main = {
    randDir;
    findFood;
    turnAround;
    goHome;
    main;
}


rec goHome = {
    if Home Here then {
        Drop;
    } else {
        Move;
        goHome;
    }
}

rec findFood = {
    if Food Here then {
        PickUp;
    } else {
        Move;
        findFood;
    }
}


turnAround = { for [1,1,1] { Turn Left; } }


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

