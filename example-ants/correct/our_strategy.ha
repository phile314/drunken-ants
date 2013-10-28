main = {
    explore;
--    goHome;
--    main;
}


rec explore = {
    let f td1 td2 = {
        mark 5 3;
        Turn td1;
        Move;
        Turn td1;
        mark 5 2;
        Turn td2;
        Move;
        Turn td2;
        mark 5 1;
        Turn td1;
        Move;
        Turn td1;
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

mark o i = {
    Mark o;
    Move;
    Mark i;
    Move;
    Mark o;
}
