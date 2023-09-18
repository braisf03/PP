(*Función que calcula el factorial de un número*)
let rec fact = function
	0 -> 1
	| n -> n * fact (n - 1)

let num = Array.length Sys.argv

let main =
    if num <> 2 then print_endline "Número de argumentos incorrecto, deben ser 2."
    else print_endline(string_of_int(fact(int_of_string(Array.get Sys.argv(1)))))
	
