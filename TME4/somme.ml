(*TD 4*)

type value = B of bool | I of int;;
type 'a option = None | Some of 'a;;
exception TYPE_ERROR of int;;
exception DIV_BY_0 of int;;

let not1(v:value):value =
  match v with
    | B x -> B (not x)
    | I y -> raise(TYPE_ERROR y)
;;

let not2(v:value) =
  match v with
    | I 0 -> not1 (B false)
    | I _ -> not1 (B true)
    | B x -> not1 (B x)
;;

let div1 (v1: value)(v2:value) :value =
  match v1 with
    | B _ -> raise(Invalid_argument "div")
    | I x -> match v2 with 
              | B _ -> raise(Invalid_argument "div")
              | I 0 -> raise(DIV_BY_0 x)
              | I y -> I (x/y)
;;

let div2 (v1: value)(v2:value) :value option =
  try Some (div1 v1 v2) with 
    | DIV_BY_0 v2-> None

;;

type 'a btree = 
  | Empty
  | Node of ('a * 'a btree * 'a btree)
;;

let rec max_length_branch (t: 'a btree): 'a list =
  match t with 
    | Empty -> []
    | Node(e,g,d) -> let l1 = max_length_branch g in 
      let l2 = max_length_branch d in 
        if ((List.length l1) >= (List.length l2)) then e::l1
        else e :: l2
;;

let rec etiq_prof_list (x:'a)(t:'a btree): int list =
  match t with
    | Empty -> []
    | Node(e,g,d) -> if (e=x) then (etiq_prof_list x Empty)
      else let l1 = e:: (etiq_prof_list x g) in 
              let l2 = e::(etiq_prof_list x d) in 
                [List.length l1] @ [List.length l2]
;; 

(*TME 4*)

let rec lt_btree (bt: 'a btree) (x:'a) : bool =
  match bt with 
    | Empty -> true
    | Node (e,g,d) -> (e < x) && (lt_btree g x) && (lt_btree d x)
;;

let rec ge_btree (bt: 'a btree) (x:'a) : bool =
  match bt with 
    | Empty -> true
    | Node (e,g,d) -> (e >= x) && (ge_btree g x) && (ge_btree d x)
;;

let rec is_abr (bt: 'a btree) :bool =
  match bt with
    | Empty -> true
    | Node (e,g,d) -> (lt_btree g e) && (ge_btree d e) && (is_abr g) && (is_abr d)
;;

let rec mem (bt: 'a btree) (x: 'a) =
  match bt with
    | Empty -> false
    | Node (e,g,d) -> (e = x) || (mem g x) || (mem d x)
;;

(* Exemple de pire cas : Arbre de grande taille dans laquelle l'etiquette
recherché est au bout de celle-ci*)

let rec insert (bt: 'a btree) (x: 'a) : 'a btree =
  if (is_abr bt) then
    match bt with
      | Empty -> Node (x , Empty , Empty)
      | Node (e,g,d) -> if (x < e) then Node (e , (insert g x) ,d)
        else Node( e , g ,(insert d x))
  else
    Empty
;;

let rec abr_of_list (l: 'a list) : 'a btree =
  match l with 
    | [] -> Empty
    | h::t -> (insert (abr_of_list t) h) 
;;

let rec list_of_abr (bt: 'a btree) : 'a list = 
  match bt with
    | Empty -> []
    | Node (e,g,d) -> (list_of_abr g) @ e :: (list_of_abr d)
;;

let abr_sort (l: 'a list) : 'a list =
  (list_of_abr (abr_of_list l))
;;