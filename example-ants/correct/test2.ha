let turnBack = 
  for [1,2,3] turn

let goHome = {
  move
  if ! (Home Here) then goHome
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
