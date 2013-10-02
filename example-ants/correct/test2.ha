let turnBack = { for [1,2,3] turn } 
    goHome = {
      move
      if !(Home Here) then goHome
      } in {
  let findFood = {
    move 
    try pickUp with {
      turnBack
      goHome
      drop
      findFood
    }
    catch
      findFood
  }
