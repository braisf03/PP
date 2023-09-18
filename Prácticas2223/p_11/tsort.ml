open Bin_tree;;

let insert_tree ord x t =
  let rec aux = function
    | Empty -> Node (x, Empty, Empty)
    | Node (y, left, right) ->
      if ord x y then Node (y, aux left, right)
      else if ord y x then Node (y, left, aux right)
      else Node (y, left, right)
  in aux t;;
      
let tsort ord l =
  inorder (List.fold_left (fun a x -> insert_tree ord x a) Empty l);;

