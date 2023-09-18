(*Esta función va metiendo números desde 0 hasta n y luego le da la vuelta a la lista*)
let to0from n =
   let rec aux i x =
    if i < 0 then x
    else aux (i - 1) (i :: x)
   in List.rev (aux n [])
  
(*Esta función recibe dos números m y n, y va rellenando una lista desde m hasta n*)  
let fromto m n =
    let rec aux l i =
     if i < m then l
     else aux (i::l) (i - 1)
    in aux [] n
	
(*Esta función recibe una lista de elementos y va haciendo la suma acumulada de esta en cada elemento*)
let incseg l =
  let rec aux l acc l2 =
   match l with
    [] -> []
    | [h] -> List.rev ((h + acc)::l2)
    | h::t -> aux t (h + acc) ((h + acc)::l2)
  in aux l 0 [];;
  
(*Esta funcion recibe un número x y una lista l y devuelve otra lsita so el elemento x*)
let remove x l =
  let rec aux acc = function
   [] -> l
   | h::t -> if x = h then List.rev_append acc t
  else aux (h::acc) t
 in aux [] l ;;

(*Esta función recibe una lista l y devuelva la lista sin los elementos repetidos consecutivos*)
let compress l =
  let rec aux acc l = match l with
   | h1::h2::t -> if h1=h2 then aux acc (h2::t)
  else aux (h1::acc) (h2::t)
   | [h] -> aux (h::acc) []
   | []-> List.rev acc
 in (aux [] l);;
 
