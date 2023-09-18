let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

(* uncurry (+); *)
(* Devolverá lo que hace uncurry *)

let sum = (uncurry (+))
(* Se almacena en sum lo que hace uncurry *)

(* sum 1;; *)
(* Devolverá un error porque necesita dos parámetros *)

(* sum (2,1); *)
(* Devolverá 3*)

let g = curry (function p -> 2 * fst p + 3 * snd p)
(* Almacena en g, lo que queremos hacer con curry *)

(* g (2,5); *)
(* Debería dar un error por no recibir el tipo de dato correcto *)

let h = g 2
(* En h, metemos la operación de g 2 *)

(* comp : ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b) *)
let comp = function f -> function g -> function c -> f (g c)

let f = let square x = x * x in comp square ((+) 1)

let i = function a -> a;;
let j = function (a, b) -> a;;
let k = function (a, b) -> b;;
let l = function a -> [a];;
