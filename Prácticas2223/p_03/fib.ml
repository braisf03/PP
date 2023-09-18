(*Fibonacci recursivo*)
let rec fib n =
if n <= 1 then n
else fib (n-1) + fib (n-2)

let rec calcular n =
	if n = 0 then "0"
	else calcular(n-1)^"/n"^string_of_int(fib(n));;
	
let rec escribir =
	if (Array.length Sys.argv) =2 then ( calcular(int_of_string(Sys.argv.(1))))
	else ("Numero de argumentos incorrecto, deben ser 2.") 
	in print_endline escribir;;
