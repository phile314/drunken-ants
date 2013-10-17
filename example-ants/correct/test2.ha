let turnBack = { for [1,2,3] turn }, 
    goHome = move ,
    findFood = {
     move;
     try {
       pickUp;
       turnBack;
        goHome;
        drop;
       findFood
     }
     catch
       findFood } 
  in
    findFood
