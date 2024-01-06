(* Práctica 9 *)  

type 'a bintree = Empty | Node of 'a * 'a bintree * 'a bintree;;

(*
let t =
  Node (5,
        Node (3, Node (2, Empty, Empty), Node (4, Empty, Empty)),
        Node (7, Node (10, Empty, Empty), Node (8, Empty, Empty)));;
*)



(* in_order : 'a bintree -> 'a list *)
let in_order tree = 
  let rec aux l arb = match arb with
    | Empty -> l
    | Node (r,i,d) -> aux ( r :: aux l d) i
  in aux [] tree
;;


(* insert : ('a -> 'a -> bool) -> 'a bintree -> 'a -> 'a bintree *) 
let rec insert f tree x = match tree with 
  | Empty -> Node (x,Empty,Empty)
  | Node (r,i,d) -> if f x r then Node(r,insert f i x,d) else Node(r,i,insert f d x)
;;

(* bst : ('a -> 'a -> bool) -> 'a list -> 'a bintree *)
let bst f l = 
  let rec aux l tree = match l with
    | [] -> tree 
    | h::t -> aux t (insert f tree h)
  in aux l Empty
;;

(* qsort : ('a -> 'a -> bool) -> 'a list -> 'a list *)
let qsort f l = in_order (bst f l);;