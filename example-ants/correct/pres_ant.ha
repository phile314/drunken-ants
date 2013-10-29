import module_a
dir = Left
rec main = {
    try {
        Move;    Turn dir;    PickUp;    main;
    } catch {
        g;
    }
}
rec g = {
    if Home Here && Friend Ahead then {
        let s = { Drop; }
        in {
            s; g;
        }
    } else {
        for x in [Left, Right] {
            Turn x;
        }
        main;
    }
}
    
