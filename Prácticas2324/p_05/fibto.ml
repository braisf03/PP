let rec fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2);;

let calcular n = 
  let rec aux x = 
    let f = fib(x) in if f > n then ()
    else (let _ = print_endline(string_of_int(f)) in aux (x+1))
  in aux 0;;
  
let () = 
  if Array.length Sys.argv <> 2 then raise (Failure "Parámetros incorrectos")
  else calcular (int_of_string(Sys.argv.(1)));;

