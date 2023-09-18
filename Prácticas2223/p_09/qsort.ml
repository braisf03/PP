(*Esta funcion se urará para generar números aleatorios*)
let rec generar_lista_aleatoria n aux max =
  if n = 0 then aux
  else generar_lista_aleatoria (n - 1) (Random.int max :: aux) max;;


let rec qsort1 ord = function
	[] -> []
	| h::t -> let after, before = List.partition (ord h) t 
 in qsort1 ord before @ h :: qsort1 ord after;;
(*Cuando la lista es desbalanceada el algoritmo no será bueno*)

let rec qsort2 ord =
	let append' l1 l2 = List.rev_append (List.rev l1) l2 in function
	[] -> []
	| h::t -> let after, before = List.partition (ord h) t 
 in append' (qsort2 ord before) (h :: qsort2 ord after);;

(*qsort2 es más rápido cuando la lista ya está ordenada
qsort2 permite ordenar listas más grandes sin dar stack overflow*)

  
let l1 = generar_lista_aleatoria 500000 [] 500000;;

(*qsort2 es más lento que qsort1 cuando la lista está inicializada
aleatoriamente o inversamente.*)
