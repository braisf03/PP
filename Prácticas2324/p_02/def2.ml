let p = function x -> 2. *. x *. 2. *. asin 1.

let area = function x -> x *. x *. 2. *. asin 1.

let absf = function x -> if x > 0. then x else (-1.) *. x

let even = function x-> x mod 2 = 0

let rec next3 = function x -> if x mod 3 = 0 then x else next3 ( x + 1 )

let is_a_letter = function x -> 
	if 65 <= int_of_char x && int_of_char x <= 90 then true 
	else if 97 <= int_of_char x && int_of_char x <= 122 then true 
	else false
	
let string_of_bool = function x -> if x then "verdadero" else "falso"
