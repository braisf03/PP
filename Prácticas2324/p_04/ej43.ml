(* Para hacer la funcion reverse me he apoyado en las funciones num_cifras y pow*)
let rec num_cifras n=
	if n = 0 then 0
	else 1 + num_cifras(n / 10)

(*Se va multipicando por 10 hasta que n sea 0*)
let rec exp10 n =
if n >= 0 then
	if n = 0 then 1
	else 10*exp10(n-1)
else 0;;

(*Utiliza las funciones anteriores y a sí misma para realizar un giro del número*)
let rec reverse n =
if n >= 0 then
	if n = 0 then 0
	else (n mod 10) * exp10 (num_cifras n - 1) + reverse (n / 10)
else 0;;

(*------------------------*)

let palindromo x =
    let rec aux x i =
        if i >= ((String.length x) / 2) then true 
        else if x.[i] <> x.[String.length x - 1 - i] then false
             else aux x (i+1) 
    in aux x 0;;

(*------------------------*)

let rec mcd (x,y) = if x = 0 then y else 
    if x > y then mcd (x mod y,y) else mcd (y,x);;