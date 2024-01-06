let rec sumto x = 
    if x = 0 then 0 
    else x + sumto (x-1);;

let rec exp10 x = 
    if x = 0 then 1 
    else 10 * exp10 (x-1);;

let rec num_cifras x = 
    if -10 < x && x < 10 then 1 
    else 1 + num_cifras (x / 10);;

let rec sum_cifras x = 
    if -10 < x && x < 10 then abs x 
    else abs (x mod 10) + sum_cifras ( x / 10);;
