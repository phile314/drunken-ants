f x =
  try move with
    move
  catch
    turn left

main = f 10
  if enemy ahead then
     move
  else
     turn right

