(* Práctica 8 *)

let come (i1,j1) (i2,j2) =
  i1 = i2 || j1 = j2 || abs (i1-i2) = abs(j1-j2);;

let compatible p l =
  not (List.exists (come p) l);;

let queens n =
  let rec completa path i j =
    if i > n then [path]
    else if j > n then []
    else if compatible (i, j) path 
    then List.append (completa ((i, j) :: path) (i + 1) 1)  (completa path i (j + 1))
    else completa path i (j + 1) 
  in completa [] 1 1;;

let is_queens_sol n l = 
  let rec aux l1 = match l1 with
      [] -> if n <> 2 || n <> 0 then true else false 
    | (x,y)::[] -> if (x > n || y > n ) then false else true
    | (x,y)::(z,w)::t ->  if (x > n || y > n || z > n || w > n) then false 
        else if come (x,y) (z,w) then false
        else if List.length t = 1 then (if come (z,w) (List.hd t) then false else aux t)
        else aux t
  in aux l;;