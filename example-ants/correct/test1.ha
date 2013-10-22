f x = {
  try move with
    move
  catch
    turn left
}

main = {
  f 10;
  if (Enemy Ahead) then
     move
  else
     turn right
}
