main = {
    f True
}


f x = {
    if x then {
        Move
    } else {
        Turn
    };
    f !x
}
