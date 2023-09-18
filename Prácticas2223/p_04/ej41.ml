(*Se va realizando un mod 10 y cuando llegue a 0 el número se sacan la suma total*)
let rec sum_cifras n=
if n >=0 then 
	if n=0 then 0
	else n mod 10+sum_cifras(n/10)
else 0;;

(*Se va realizando un mod 10 y cuando llegue a 0 se saca el número de cifras*)	
let rec num_cifras n=
	if n=0 then 0
	else 1+num_cifras(n/10)

(*Se va multipicando por 10 hasta que n sea 0*)
let rec exp10 n =
if n>=0 then
	if n=0 then 1
	else 10*exp10(n-1)
else 0;;

(*Utiliza las funciones anteriores y a sí misma para realizar un giro del número*)
let rec reverse n =
if n>=0 then
	if n=0 then 0
	else (n mod 10)*exp10(num_cifras n - 1)+reverse (n / 10)
else 0;;

(*Realiza comparaciones para determinar si se leen de misma manera de adelate para atras*)
let rec palindromo s = 
	let rec counter i =
		if i >= (String.length s-i) then true
		else if s.[i] <> s.[String.length s - i - 1] then false
			else counter (i+1)
	in counter 0
