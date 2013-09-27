let turnBack = 
  for [1..6] turn   -- [from..to] is not accepted by the lexer

let goHome =
  move
  if !(Here, Home) then goHome    -- Capitalized tokens are refused
   

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
