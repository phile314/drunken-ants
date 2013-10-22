f x = {
  try move with
    move
  catch
    turn left
}

main = {
  f 10;
  if (Foe Ahead) then
     move
  else
     turn right
}
