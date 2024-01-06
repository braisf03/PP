(* Práctica 7 *)
(* Funciones iniciales *)

let rec insert x = function
    [] -> [x]
  | h::t -> if x <= h then x :: h :: t
      else h :: insert x t;;

let rec isort = function
    [] -> []
  | h::t -> insert h (isort t);;

(* Lista que da Stack overflow *)

let bigl = List.init 200_000 (function x -> 2 * x);; 

(* insert_t: ‘a -> ‘a list -> ‘a list *) 

let insert_t x l =
  let rec aux acc l = match l with
    | [] -> List.rev (x::acc)
    | h::t -> if x <= h then List.rev_append acc (x::h::t)
        else aux (h::acc) t 
  in aux [] l;;


(* isort_t: ‘a list -> ‘a list *)

let isort_t lst = 
  let rec aux acc l = match l with
    | [] -> acc
    | h :: t -> aux (insert_t h acc) t
  in aux [] lst;;

(* rlist: int -> int list *)

let rlist = fun x -> List.init x (fun _ -> Random.int 400_000);;

(* crono : ('a -> 'b) -> 'a -> float *)

let crono f x =
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t;;


(* LISTAS *)

let lc1 = List.init 10_000 (function x -> 2 * x);;

let lc2 = List.init 20_000 (function x -> 2 * x);;

let ld1 = List.rev lc1;;

let ld2 = List.rev lc2;;

let lr1 = rlist 10_000;;

let lr2 = rlist 20_000;;

(*

Como se puede observar aqui las listas que usan 
isort coinciden con todas las que usan isort_t.

# (isort lc1) = (isort_t lc1);;
- : bool = true
# (isort lc2) = (isort_t lc2);;
- : bool = true
# (isort ld1) = (isort_t ld1);;
- : bool = true
# (isort ld2) = (isort_t ld2);;
- : bool = true
# (isort lr1) = (isort_t lr1);;
- : bool = true
# (isort lr2) = (isort_t lr2);;
- : bool = true

   
///////Tiempos de isort.///////

# crono isort lc1;;
- : float = 0.000762000000001705757

# crono isort lc2;;
- : float = 0.00101599999999990587

# crono isort ld1;;
- : float = 1.3618409999999983

# crono isort ld2;;
- : float = 6.63163500000000283

# crono isort lr1;; 
- : float = 0.707706000000001723

# crono isort lr2;;
- : float = 3.16024399999999872

///////Tiempos de isort_t.//////

# crono isort_t lc1;;
- : float = 2.04068000000000183

# crono isort_t lc2;;
- : float = 9.926167

# crono isort_t ld1;;
- : float = 0.000831999999995503

# crono isort_t ld2;;
- : float = 0.00117099999999936699

# crono isort_t lr1;;
- : float = 0.946870999999994467

# crono isort_t lr2;;
- : float = 4.5412369999999882

En el ascendente podemos apreciar que los tiempos sufren un cambio bastante drástico, isort normal
esta en su mejor caso haciendo tiempos pequeños, mientras que la recursiva final da unos tiempos 
muy altos, Esto se puede deber a las comprobaciones dentro de la función terminal.

En el descendente se puede apreciar que los tiempos mejoran de la versión no terminal a la recursiva
final. Esto se puede deber a que estamos ante el peor caso de inserccion en la lista descendente y la
recursiva final trabaja mejor en este caso.

En el aleatorio no ase aprecia ninguna diferencia en los tiempos de la lista de 10_000 elementos,
apenas de 
pero el la lista de 20_000 si que se puede apreciar un cambio notable, ya que pasamos del mejor caso
al peor en la versión recursiva terminal. Esto se puede deber al volteado de la lista al final de 
cada caso.

*)


(* isort_g: (‘a -> ‘a -> bool) -> ‘a list -> ‘a list  *) 


let isort_g f lst = 
  let rec insert acc x = function
    | [] -> List.rev (x :: acc)
    | h :: t ->
        if f x h then List.rev_append acc (x :: h :: t)
        else insert (h :: acc) x t
  in 
  let rec isort_a acc l = match l with
    | [] -> List.rev acc
    | h::t -> isort_a (insert [] h acc) t
  in isort_a [] lst
;;


let bigl2 = List.init 200_000 (function x -> 2 * x);; 

let split_t l = 
  let rec aux l1 l2 l3 = match l1 with
    | [] -> List.rev l2,List.rev l3
    | h::[] -> aux [] (h::l2) l3
    | h1::h2::t -> aux t (h1::l2) (h2::l3)
  in aux l [] []
;; 
          
let merge_t (la,lb) = 
  let rec aux l1 l2 l3 = match l1,l2 with
    | [],[] -> List.rev l3
    | [],h::t | h::t,[] -> aux [] t (h::l3)
    | h1::t1,h2::t2 -> if h1 <= h2 then aux t1 l2 (h1::l3)
        else aux l1 t2 (h2::l3)
  in aux la lb []
;;

let msort' lst =
  let rec aux acc l = match l with
    | [] | _::[] -> List.rev_append acc l
    | _ -> let l1, l2 = split_t l in
        let merged = merge_t (aux [] l1,aux [] l2) in
        List.rev_append acc merged
  in aux [] lst
;;

let bigl3 = [];; 

(*

Tiempos de msort'.   

# crono msort' lc1;;
- : float = 0.0188830000000024256
# crono msort' lc2;;
- : float = 0.0339869999999962147
# crono msort' ld1;;
- : float = 0.0198070000000001301
# crono msort' ld2;;
- : float = 0.0337689999999994939
# crono msort' lr1;;
- : float = 0.018760000000000332
# crono msort' lr2;;
- : float = 0.0294020000000045911

Como se pueden observar, los tiempos son inferiores a los de insercción por lo que
se llega a la conclusión de que el algoritmo de mergesort es muchísimo más eficiente
y rápido. Además parece que los tiempos son casi lineales.


msort vs msort'.

# crono msort lr1;;
- : float = 0.0149880000000095492
# crono msort lr2;;
- : float = 0.0324829999999991514
# crono msort' lr1;;
- : float = 0.018139999999995382
# crono msort' lr2;;
- : float = 0.0342040000000025657

Como se puede apreciar los tiempos son bastante similares en los dos casos, aunque 
ligeramente superiores en las versiones recursivas finales. Se puede decir que consumimos
un poco de tiempo en hacerlas terminales.

*)

let rec msort_g ord l =
  let mergeord ord (l1, l2) =
    let rec aux (a1, a2) mer = match a1, a2 with
        [], l | l, [] -> List.rev_append mer l
      | h1::t1, h2::t2 -> if ord h1 h2 then aux (t1, h2::t2) (h1::mer)
          else aux (h1::t1, t2) (h2::mer)
    in aux (l1, l2) []
  in
  match l with
    [] | _::[] -> l
  | _ -> let l1, l2 = split_t l
      in mergeord ord (msort_g ord l1, msort_g ord l2);;
