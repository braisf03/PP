(*Parte 1*)

(*Devuelve el primer elemento de la lista*)
let hd l = match l with
    | h::_ -> h
    | [] -> raise (Failure "hd");;

(*Devuelve el elemento final de la lista*)
let tl l = match l with
    | _::t-> t
    | [] -> raise (Failure "tl");;

(*Devuelve el número de elementos de la lista*)
let length l =
    let rec aux a = function
        [] -> a
        | h::t -> aux (a+1) t
    in aux 0 l;;

(*Compara las longitudes de las listas para saber cual es mas larga*)
let rec compare_lengths  l1 l2 = 
    match l1,l2 with 
        ([],[]) -> 0
        |(_,[]) -> 1
        |([],_) -> -1
        |(_::t1,_::t2) -> 
            compare_lengths t1 t2;;

(*Devuelve el n-ésimo elemento de la lista*)
let rec nth l n = 
    if(n == 0) then hd l 
    else if(n > 0) then nth (tl l)(n-1)
    else if(n < 0) then raise (Invalid_argument "List.nth")
    else raise (Failure "nth");;

(*Concatena dos listas*)
let rec append l1 l2 = 
    if l1=[] then l2
    else (hd l1)::(append(tl l1) l2);;

(*Parte 2*)

(*Busca el elemento que cumpla la condicion p*)
let rec find p = function
    [] -> raise Not_found
    | header::tail -> if p header 
	then header 
	else find p tail

(* Comprueba que todos los elementos de la lista están cumpliendo la condicón p *)
let rec for_all p = function
    [] -> true
    | header::tail -> (p header) && (for_all p tail)
	
(* Comprueba que al menos uno de los elementos de la lista cumpla la función p *)
let rec exists p = function
	[] -> false
	| header::tail -> p header || exists p tail	

(* Comprueba si el elemento n está en la lista *)		
let rec mem n = function
    [] -> false
    | header::tail -> 
		if(n = header) 
		then true
		else (mem n tail);;

(* Devuelve una lista donde todos los elementos cumplen la condición p *)
let rec filter p = function
	[] -> []
	| header::tail ->
		if(p header)
			then header::(filter p tail)
			else (filter p tail)

(* Hace lo mismo que filter*)
let find_all p = filter p

(* Devuelve dos listas, una con los elementos que satisfacen p y otra donde no se cumplen *)
let rec partition p = function
	[] -> ([],[])
	| header::tail -> 
	let (satisface, noSatisface) = partition p tail
	in if (p header) 
		then (header::satisface, noSatisface)
		else (satisface, header::noSatisface)
	
(* Transforma una lista en dos listas, una con las primeras parejas
y otra con las segundas parejas *)
let rec split = function
	[] -> ([],[])
	| (header1, header2)::tail ->
		let tail1, tail2 = split tail
		in header1::tail1, header2::tail2

(* Se le pasan dos listas y devuelve parejas formadas entre las dos *)
let rec combine l1 l2 =
	match (l1, l2) with
	[], [] -> []
	| header1::tail1, header2::tail2 -> 
		(header1, header2) :: (combine (tail1) (tail2))
		| _ -> raise (Invalid_argument "combine");;

(* Devuelve la lisa invertida *)
let rec rev = function
    [] -> []
    | head::tail -> append (rev tail) [head]

(* Comprueba desde 0 hasta n si cumplen la función f *)
let init n f = 
    if n < 0 then raise (Invalid_argument "init")
    else let rec aux acc i =
        if i = n then rev acc
        else aux (f i::acc) (i+1)
    in aux[] 0

(* Le da la vuelta a la primera lista y la concatena con la segunda *)
let rev_append l1 l2 =
	if l1 = [] then l2
	else append (rev l1) l2

(* Concatena una lista de listas en el orden que son introducidas *)
let rec concat = function
	[] -> []
	| head::tail -> append head (concat tail)
	
(* Hace lo mismo que concat *)
let flatten = concat
	
(* Ejecuta una función sobre cada elemento de la lista *)
let rec map f = function
    [] -> []
    | head::tail -> (f head) :: (map f tail)
	

let rev_map f l = rev (map f l)
	
let rec map2 f l1 l2 =
    if (length l1 != length l2)
        then raise (Invalid_argument "map2")
    else if (length l1 == 0) then []
    else (f(hd l1)(hd l2)) :: map2 f (tl l1) (tl l2)

let rec fold_left f a = function
    [] -> a
    | head::tail -> fold_left f (f a head) tail

let rec fold_right f l a = match l with
  | [] -> a
  | head::tail -> f head (fold_right f tail a)

