let next = function 
| (1,y) -> if (1 + y) mod 2 = 0 then (1,y+1) else (1+1,y-1)
| (x,1) -> if (x + 1) mod 2 = 0 then (x-1,1+1) else (x+1,1)
| (x,y) -> if (x + y) mod 2 = 0 then (x-1,y+1) else (x+1,y-1);;

(*------------------------*)

let rec steps_from (x,y) i = 
  if i <= 0 then (x,y)
  else steps_from (next(x,y)) (i-1);;

(*------------------------*)

let pair x = steps_from (1,1) (x-1);;