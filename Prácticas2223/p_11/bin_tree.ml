type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec map_tree f = function
    Empty -> Empty
  | Node (x, left, right) -> Node (f x, map_tree f left, map_tree f right);;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;

let sum t= fold_tree (fun x y z -> x + y + z) 0 t;;

let prod t= fold_tree (fun x y z -> x *. y *. z) 1. t;;

let size t= fold_tree (fun x y z -> 1 + y + z) 0 t;;

let height tree = fold_tree (fun _ left right -> 1 + max left right) 0 tree;;

let inorder t= fold_tree (fun x y z -> y @ x::z) [] t;;

let mirror t = fold_tree (fun x left right -> Node (x, right, left)) Empty t;;
