import StdLib

main = f3

f1 = { for [1,2,3] {turn left;} }

f2 = f1

rec f3 = {
     move;
     try {
       pickUp;
       turnBack;
       goHome;
       drop;
       f2;
    } catch {f3;}
}

rec f4 x = {
      if (x) then {
        f4 !x;
      }
      else{
        f3;
      }
}
