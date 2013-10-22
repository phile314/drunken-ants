f1 = { for [1,2,3] turn }
f2 = f1
f3 = {
     move;
     try {
       pickUp;
       turnBack;
        goHome;
        drop;
       f2
    } catch f3 }
f4 x = {
      if (x) then
        f4 !x
      else
        f3 }

main = f3
