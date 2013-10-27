turnBack = {
    for [1,2,3] {
        Turn Left;
    }
}

goHome = { Move; }

findFood = {
  Move;
  try {
    PickUp;
    turnBack;
    goHome;
    Drop;
    findFood;
  } catch {
    findFood;
  }
}

main = { findFood; }
