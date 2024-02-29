
let rec map_cons(e:'a)(l:'a list list): 'a list list =
  match l with 
  | [] -> []
  | h::t -> (e :: h) :: (map_cons e t)
;;


