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

(*------------------------*)

(* pair_i repetía en todas las llamadas recursivas la funcion pair, teniendo que hacer todo el recorrido en cada llamada,
por lo que el tiempo aumentaba considerablemente. Para mejorarla se ha hecho una versión recursiva final que reusa el valor 
calculado previamente para evitar ese calculo ineficiente. *)

let pair_i' (x,y) = if (x <= 0 || y <= 0) then raise (Failure "Ni valores negativos ni 0")
else let rec aux (a,b) n =
        if x = a && y = b then n
        else aux (next(a,b)) (n+1)
      in aux (1,1) 1;;
