let is_prime n =
	let rec check_from i =
		i >= n ||(n mod i <> 0 && check_from (i+1))
	in check_from 2;;

let is_prime2 n = 
    let rec check_from i =
        (float_of_int i) >= (sqrt(float_of_int n)) +. 1. || (n mod i <> 0 && check_from (i + 1))
    in check_from 2

let rec next_prime n =
	if is_prime (n + 1) then n + 1 
	else next_prime (n + 1)
	
let rec next_prime2 n = 
	if is_prime2 (n + 1) then n + 1
	else next_prime2 (n + 1)
	
let rec last_prime_to n = 
	if is_prime n then n 
	else last_prime_to (n - 1)
	
let rec last_prime_to2 n = 
	if is_prime2 n then n 
	else last_prime_to2 (n - 1)
	
let num = Array.length Sys.argv

let main = 
    if num <> 2 then print_endline "Número de parámetros incorrecto, deben ser 2."
    else print_endline("Siguiente primo con next_prime2\t\t" ^ string_of_int( next_prime2( int_of_string( Array.get Sys.argv(1) ) ) ) );
	 print_endline("Siguiente primo con next_prime\t\t" ^  string_of_int( next_prime( int_of_string( Array.get Sys.argv(1) ) ) ) );
	 print_endline("Anterior primo con last_prime_to2\t" ^ string_of_int( last_prime_to2( int_of_string( Array.get Sys.argv(1) ) ) ) );
	 print_endline("Anterior primo con last_prime_to\t" ^  string_of_int( last_prime_to( int_of_string( Array.get Sys.argv(1) ) ) ) )
