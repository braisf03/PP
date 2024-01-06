(* Opcional 9*)
open Bintree;;

(* in_order : 'a bintree -> 'a list *)
let in_order tree = 
  let rec aux l arb = match arb with
    | Empty -> l
    | Node (r,i,d) -> aux ( r :: aux l d) i
  in aux [] tree
;;


(* is_bst : ('a -> 'a -> bool) -> 'a bintree -> bool *) 
let is_bst ord tree = 
  let arbol = in_order tree
  in let rec aux l = match l with
      | [] | _::[] -> true
      | h::t -> if ord h (List.hd t) then aux t else false
  in aux arbol
;;

(* bfs : 'a bintree -> 'a list *) 
let bfs arbol = 
  let rec aux cola acc = match cola with 
    | [] -> acc
    | h::t -> match h with
      | Empty -> aux t acc 
      | Node (v, i, d) -> aux (t @ [i] @ [d]) (acc @ [v])
  in aux [arbol] [];;

(* bfs' : 'a bintree -> 'a list *)
(* En la versión que uso (5.1) la función List.append es recursiva terminal,
   por lo que la función es recursiva terminal. En otras versiones no lo es. *)
let bfs' tree =
  let rec aux l t = match t with
    | [] -> List.rev l
    | Empty::t -> aux l t
    | Node (r, Empty, Empty)::t -> aux (r::l) t
    | Node (r, p, Empty)::t | Node (r, Empty, p)::t -> aux (r::l) (List.append t [p])
    | Node (r, i, d)::t -> aux (r::l) (List.append t [i; d])
  in aux [] [tree];;


let rec height tree = match tree with
  | Empty -> 0
  | Node(_, i, d) -> 1 + let izq = height i in
                     let der = height d in 
                     if izq > der then izq else der
;;

let rec perfecto tree = match tree with
  | Empty | Node(_, Empty, Empty) -> true
  | Node(_, Empty, _) | Node(_, _, Empty) -> false
  | Node(r, i, d) -> perfecto i && perfecto d && height i = height d
;;


let casi_completo tree = match tree with
    Empty -> false
  | Node(r,i,d) -> true;;
