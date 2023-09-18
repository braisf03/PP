
open G_tree;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

let breadth_first_t arbol =
  let rec aux acum = function
      Gt (x, []) -> List.rev (x::acum)
    | Gt (x, Gt(raiz, hijos)::lista) ->
      aux (x::acum) (Gt(raiz, List.rev_append (List.rev lista) hijos))
  in aux [] arbol;;

(*Función que crea un árbol de 200.000 elementos*)
let t2 =
  let rec hacer t2 n =
    if n <= 0 then t2
    else hacer (Gt(n, [t2])) (n-1)
  in hacer (Gt(200000, [])) 200000;;

