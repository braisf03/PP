let p = function x -> 2. *. x *. 2. *. asin 1.

let area = function x -> x *. x *. 2. *. asin 1.

let absf = function x -> if x > 0. then x else (-1.) *. x

let even = function x-> x mod 2 = 0

let next3 = function x -> if x mod 3 = 0 then x else x - x mod 3 + 3

let is_a_letter = function x -> 
	if 65 <= int_of_char x && int_of_char x <= 90 then true 
	else 97 <= int_of_char x && int_of_char x <= 122
	
let string_of_bool = function x -> if x then "verdadero" else "falso"
