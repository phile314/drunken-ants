main = {
    randDir;
    nothing;
}

rec nothing = {
    Drop;
    nothing;
}


randDir = {
    with probability 0.33 do {
        Turn Left;
    } otherwise {
        Drop;
    }
}

