let turnBack = 
  for [1,2,3,4,5,6] turn

let goHome = {
  move
  if !(here, home) then goHome
}

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
