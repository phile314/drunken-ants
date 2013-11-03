turnBack = {
  for [1..3] turn   -- [from..to] is not accepted by the lexer
}

goHome = {
  move
  if !(Home Here) then goHome
}

findFood = {
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
