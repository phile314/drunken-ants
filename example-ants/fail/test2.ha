let turnBack = 
  for [1..3] turn   -- [from..to] is not accepted by the lexer

let goHome =
  move
  if !(Home Here) then goHome
   

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
