(*Si y y x son mayores o iguales a 0 se aplica euclides y se determina el mcd*)
let rec mcd (x, y)=
if x>=0 && y>=0 then 
	if x = 0 then y
	else mcd(y mod x, x)
else 0
