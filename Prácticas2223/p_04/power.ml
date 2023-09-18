(*Se multiplica x por la siguiente iteración de y-1*)
let rec power x y = 
if y>=0 then
	if y=0 then 1
	else x*power x (y-1)
else 0;;

(*Según si y es par o no se ejecuta una relación distinta*)
let rec power' x y =
if y>=0 then
	if y=0 then 1
	else if (y mod 2=0) then power' (x*x) (y/2)
	else power' (x*(x*x)) (y/2)
else 0;;

(*Para x flotante solo hay q cambiar y poner puntos*)
let rec powerf x y =
if y>=0 then
	if y=0 then 1.
	else if (y mod 2 = 0) then powerf (x*.x) (y/2)
	else x*.powerf (x*.x) (y/2)
else 0.;;

