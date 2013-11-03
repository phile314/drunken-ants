main = {
    f (Foe Ahead);
}


f x = {
    if x then {
        Move;
    } else {
        Turn Left;
    }
    f !x;
}
