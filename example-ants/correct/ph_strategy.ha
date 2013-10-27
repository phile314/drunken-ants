
main = {
    explore;
}

rec explore = {
    Move;
    if Foe Ahead then {
        handleEnemy;
    } else {
        if Food Ahead then {
            handleFood;
        }
    }
    explore;
}

handleEnemy = {

    if Friend LeftAhead && Friend RightAhead then {
        PickUp;
    } else {
        runAway;
    }
}

runAway = {
    for [1,1,1] {
        Turn Left;
    }
    for [1,1,1] {
        Move;
    }
}

handleFood = {

}
