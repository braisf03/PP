(* Opcional 7 *)

let to0from x = 
  let rec aux n l =
    if n = 0 then List.rev (n::l)
    else aux (n-1) (n::l)
  in aux x [];;

let fromto m n = 
  let rec aux x l = 
    if x > n then List.rev l
    else aux (x+1) (x::l)
  in aux m [];;

let remove x l = 
  let rec aux l1 l2 = match l2 with
    | [] -> l
    | h::t -> if x = h then List.rev_append l1 t
        else aux (h::l1) t
  in aux [] l;; 

let compress l = 
  let rec aux l1 l2 = match l2 with 
    | [] -> List.rev l1
    | h::[] -> List.rev (h::l1)
    | h1::h2::t -> if h1 = h2 then aux (h1::l1) t
        else aux (h1::l1) (h2::t)
  in aux [] l;;

let append' l1 l2 = 
  let rec aux fin l = match l with
    | [] -> List.rev_append fin l2
    | h::t -> aux (h::fin) t
  in aux [] l1;;

let map' f l = 
  let rec aux l1 l2 = match l2 with
    | [] -> List.rev l1
    | h::t -> aux ((f h)::l1) t
  in aux [] l;;

let fold_right' f l x =
  let rec aux l1 n = match l1 with
    | [] -> n
    | h::t -> aux t (f h n)
  in aux l x;;

let incseg l = 
  let rec aux x l1 l2 = match l1 with
    | [] -> List.rev l2
    | h::t -> aux (x + h) t ((x + h)::l2) 
  in aux 0 l [];;
