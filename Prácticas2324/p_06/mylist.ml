(* Módulo List *)

(* val hd: 'a list -> 'a *)

let hd l = match l with
|  [] -> raise (Failure "hd")
|  h::_ -> h;;

(* val tl: 'a list -> 'a list *)

let tl l = match l with 
| [] -> raise (Failure "tl")
| h::[] -> []
| _::t -> t;;

(* val length: 'a list -> int *)

let rec length = function
[] -> 0
| _::t -> 1 + length t;;

let length l = 
  let rec aux (l1,x) = match l1 with
  | [] -> x
  | h::t -> aux(t,x+1) 
  in aux (l,0);;

(* val compare_lengths : 'a list -> 'b list -> int *)

let rec compare_lengths l1 l2 = match (l1, l2) with
| ([], []) -> 0
| ([], _) -> -1
| (_, []) -> 1
| (_::t1, _::t2) -> compare_lengths t1 t2;;

(* val compare_length_with : 'a list -> int -> int *)

let rec compare_length_with l n = match l with
  | [] -> if n = 0 then 0 else if n > 0 then -1 else 1
  | _ :: t -> if n <= 0 then 1 else compare_length_with t (n-1);;

(* val init : int -> (int -> 'a) -> 'a list *)

let init x f = if x < 0 then raise (Invalid_argument "List.init")
  else 
    let rec aux (a,l) =
      if a < 0 then l 
      else aux (a-1,((f a)::l))
    in aux (x-1,[]);;

(* val nth : 'a list -> int -> 'a *)

let nth l x = 
  if x < 0 then raise (Invalid_argument "List.nth")
  else let rec aux (l1,a) = match (l1,a) with
  | ([],_) -> raise (Failure "nth")
  | (h::_, 0) -> h
  | (_::t, x) -> aux (t,x-1)
in aux (l,x);; 

(* val append : 'a list -> 'a list -> 'a list *)
(* not tail recursive *)

let rec append list1 list2 =
  match list1 with
  | [] -> list2
  | h::t -> h :: (append t list2);;

(* val rev_append : 'a list -> 'a list -> 'a list *)

let rec rev_append l1 l2 = match l1 with
  | [] -> l2
  | (h::t) -> rev_append t (h::l2);;


(* val rev : 'a list -> 'a list *)

let rev l =
  let rec aux (l1,l2) = match (l1,l2) with 
  | ([],_ ) -> l2
  | (h::t,_) -> aux (t,h::l2)
in aux (l,[]);;
    
(* val concat : 'a list list -> 'a list *)
(* not tail recursive *)

let rec concat l = match l with
  | [] -> []
  | h::t -> append h (concat t);;

(* val flatten : 'a list list -> 'a list *)
(* not tail recursive *)

let flatten = concat;;

(* val split : ('a * 'b) list -> 'a list * 'b list *)
(* not tail recursive *)

let rec split l = match l with 
| [] -> ([],[])
| ((a,b)::t) -> let (l1,l2) = split t in (a::l1,b::l2);;

(* val combine : 'a list -> 'b list -> ('a * 'b) list *)
(* not tail recursive *)

let rec combine l1 l2 = match (l1,l2) with 
| ([],[]) -> []
| (h1::t1,h2::t2) -> (h1,h2) :: combine t1 t2
| _ -> raise (Invalid_argument "List.combine")

(* val map : ('a -> 'b) -> 'a list -> 'b list *)
(* not tail recursive *)

let rec map f list = match list with 
  | [] -> []
  | h::t -> f h :: map f t;;

(* val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list *)
(* not tail recursive *)

let rec map2 f l1 l2 = match (l1,l2) with 
| ([],[]) -> []
| (h1::t1,h2::t2) -> f h1 h2 :: map2 f t1 t2
| _ -> raise (Invalid_argument "List.map2");;

(* val rev_map : ('a -> 'b) -> 'a list -> 'b list *)

let rev_map f list =
  let rec aux (l1,l2) = match l1 with 
  | [] -> l2
  | (h::t) -> aux (t,f h::l2)
in aux (list,[]);;

(* val for_all : ('a -> bool) -> 'a list -> bool *)

let rec for_all f l = match l with 
| [] -> true 
| (h::t) -> (f h) && (for_all f t)
;;

(* val exists : ('a -> bool) -> 'a list -> bool *)

let rec exists f l = match l with 
| [] -> false 
| (h::t) -> (f h) || (exists f t)
;;

(* val mem : 'a -> 'a list -> bool *)

let rec mem x list = match list with
| [] -> false
| (h::t) -> (x = h) || (mem x t)
;;

(* val find : ('a -> bool) -> 'a list -> 'a *)

let find f l = 
  let rec aux l = match l with
| [] -> raise (Not_found)
| (h::t) -> if f h then h else aux t
in aux l;; 

(* val filter : ('a -> bool) -> 'a list -> 'a list *)

let filter f l =
  let rec aux l1 = match l1 with 
  | [] -> []
  | (h::t) -> if f h then h::(aux t) else (aux t)
  in aux l;;

(* val find_all : ('a -> bool) -> 'a list -> 'a list *)

let find_all f l = filter f l;;

(* val partition : ('a -> bool) -> 'a list -> 'a list * 'a list *)

let partition f l = (filter f l, filter (function x -> not (f x)) l);;

let partition f l = 
  let rec aux  l1 l2 l3 = match l1 with 
  | [] -> (rev l2,rev l3)
  | (h::t) -> if f h then aux t (h::l2) l3  else aux t l2 (h::l3)
in aux l [] [] ;;

(* val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)

let fold_left f n l = 
  let rec aux (x,l1) = match l1 with 
  | [] -> x
  | (h::t) -> aux (f x h, t)
in aux (n,l);;

(* val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
(* not tail recursive *)

let fold_right f list n = 
  let rec aux l = match l with 
  | [] -> n
  | (h::t) -> f h (aux t)
in aux list;;

(* val assoc : 'a -> ('a * 'b) list -> 'b *) 

let rec assoc key l = match l with
| [] -> raise (Not_found)
| ((a,b)::t) -> if key = a then b else assoc key t;; 

(* val mem_assoc : 'a -> ('a * 'b) list -> bool *)

let rec mem_assoc key l = match l with
| [] -> false
| ((a,_)::t) -> if key = a then true else mem_assoc key t;; 

(* val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list *)
(* not tail recursive *)

let remove_assoc key l =
  let rec aux l1 = match l1 with  
  | [] -> l1
  | ((a,b)::t) -> if key = a then t else (a,b)::aux t
in aux l;;