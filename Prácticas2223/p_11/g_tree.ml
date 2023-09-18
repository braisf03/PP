
type 'a g_tree =
  Gt of 'a * 'a g_tree list
;;

let rec size = function 
    Gt (_, []) -> 1
  | Gt (r, h::t) -> size h + size (Gt (r, t))
;;

(* devuelve la "altura", como número de niveles, de un g_tree *)
let rec height =
	let rec lista_max n = function
		  [] -> n
		| h::t ->
			let hh = height h in
			if hh > n
			then
				lista_max hh t
			else
				lista_max n t
	in function
	  Gt (_, []) -> 1
	| Gt (_, l) ->
		1 + lista_max 0 l;;

(* devuelve las hojas de un g_tree, "de izquierda a derecha" *)
let rec leaves =
	let rec f follas = function
		  [] -> follas
		| h::t -> f (follas @ leaves h) t
	in function
		  Gt (v, []) -> [v]
		| Gt (v, l) -> f [] l;;

(* devuelve la imagen especular de un g_tree *)
let rec mirror = function
	  Gt (v, []) -> Gt (v, [])
	| Gt (v, l) -> Gt (v , List.rev (List.map (mirror) l) );;

(* devuelve la lista de nodos de un g_tree en "preorden" *)
let rec preorder =
	let rec bucle l = function
		  [] -> l
		| h::t -> bucle ( l @ (preorder h) ) t
	in function
	  Gt (v, []) -> [v]
	| Gt (v, l) -> v::bucle [] l;;

(* devuelve la lista de nodos de un g_tree en "postorden" *)
let rec postorder =
	let rec bucle l = function
		  [] -> l
		| h::t -> bucle ( l @ (postorder h) ) t
	in function
	  Gt (v, []) -> [v]
	| Gt (v, l) -> (bucle [] l) @ [v];;

