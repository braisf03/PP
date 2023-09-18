let rec power' x y =
if y>=0 then
	if y=0 then 1
	else if (y mod 2=0) then power' (x*x) (y/2)
	else power' (x*(x*x)) (y/2)
else 0;;

let rec powmod m b e =
    let resto = b mod m
	in if e > 0 then if (e mod 2) = 0 then powmod m (resto * resto) (e / 2) mod m
		else resto * powmod m (resto * resto) ((e - 1) / 2) mod m
	else 1;;
