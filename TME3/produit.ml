let rec is_in (e : 'a)(l: 'a list) : bool = 
  match l with 
    | [] -> false
    | h::t -> if (h = e) then true else is_in e t
;;

let add_elem (e :'a)(l: 'a list) : 'a list =
  if ((is_in e l) = true) then l 
  else e::l
;;

let rec is_subset_rec (l1: 'a list)(l2 : 'a list) : bool =
  match l1 with
  | [] -> true
  | h::t -> if ((is_in h l2) = true) then is_subset_rec t l2
    else false
;;

let is_subset (l1: 'a list)(l2 : 'a list) : bool =
  List.for_all (fun x -> is_in x l2) l1
;;

let rec eq_set (l1: 'a list) (l2: 'a list) : bool =
  (is_subset l1 l2) && (is_subset l2 l1)
;;

let rec intersection_rec (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with
    | [] -> []
    | h::t -> if (is_in h l2) then h::(intersection_rec t l2)
      else (intersection_rec t l2)
;;

let intersection (l1 : 'a list) (l2 : 'a list) : 'a list =
  List.filter (fun x -> is_in x l1) l2
;;

let rec union_rec (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with
    | [] -> l2
    | h::t -> add_elem h (union_rec t l2)
;;

let union_left (l1 : 'a list) (l2 : 'a list) : 'a list =
  (List.fold_left (fun x y -> add_elem y x ) l2 l1)
;;

let union_right (l1 : 'a list) (l2 : 'a list) : 'a list =
  (List.fold_right (fun x y -> add_elem x y ) l1 l2)
;;
let rec make_pairs (x : 'a) (l : 'b list) : ('a * 'b) list =
  match l with 
    | [] -> []
    | h::t -> (x,h) :: (make_pairs x  t)
;;

let rec product_rec (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  match l1 with
    | [] -> []
    | h::t -> (make_pairs h l2) @ (product_rec t l2)
;;

let product (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list = 
  (List.flatten (List.map (fun x -> make_pairs x l2) l1))
;;

let rec powerset_rec (l : 'a list) : 'a list list =
  match l with
    | [] -> [[]]
    | h::t ->  let lr = (powerset_rec t) in
      List.map (fun l -> h::l) lr @ lr
;;


(*let powerset (l : 'a list) : 'a list list =
  List.fold_left (fun x y -> );;
*)