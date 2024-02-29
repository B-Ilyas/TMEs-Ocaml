(*Fonction vue en TD a utiliser en TME*)
let rec rm_pref (n:int)(l:'a list): 'a list =
  if (n > (List.length l)) then []
  else match l with
    | [] -> []
    | h::t -> if (n=0) then h::(rm_pref n t)
      else (rm_pref (n-1) t)
;;

let rec lg_prefix (l : 'a list) : int =
  match l with
    | [] -> 0
    | [x] -> 1
    | h::ht::t -> if (h = ht) then 1 + lg_prefix(ht::t)
      else 1
;;

let rec range_inter (a:int)(b:int) :int list =
  if a > b then []
  else a::(range_inter (a+1) b)
;;

let rec abs_list(l:int list) :int list =
  match l with
    | [] -> []
    | h::t -> (abs h) :: (abs_list t)
;;

let rec max_list (l:int list):int =
  match l with
    | [] -> raise(Invalid_argument "empty list")
    | [x] -> x
    | h::t -> max h (max_list t)
;;

let rec rm_last(l: 'a list):'a list =
  match l with
    | [] -> raise(Invalid_argument "empty list")
    | [x] -> []
    | h::t -> h::(rm_last t)
;;


(*Ex 2.1*)
let rec next_list (l :int list) :int list =
  match l with 
    | [] -> []
    | [x] -> 1::[x]
    | h::t -> (lg_prefix l)::h::(next_list (rm_pref (lg_prefix l) l))
;;

(* Operateur == ne marche pas sur les listes *)
assert ((next_list [1]) = [1;1]);;
assert ((next_list [1;2;1;1]) = [1;1;1;2;2;1]);;
assert ((next_list [1;1;1;2;2;1]) = [3;1;2;2;1;1]);;

let rec make_list_k (k:int) : int list =
  if (k == 1) then [1]
  else next_list(make_list_k(k-1))
;;

assert ((make_list_k 1) = [1]);;
assert ((make_list_k 2) = [1;1]);;
assert ((make_list_k 6) = [3;1;2;2;1;1]);;



(*Ex 2.2*)

let genere_list (n:int) : int list =
  if (n = 1) then []
  else (range_inter 2 n)
;;

assert ((genere_list 1) = []) ;;
assert ((genere_list 4) = [2;3;4]) ;;

let rec elimine (l: int list) (n:int) : int list =
  match l with
    | [] -> []
    | [x] -> if ((x mod n) == 0) then []
      else [x]
    | h::t -> if ((h mod n) == 0) then (elimine t n)
      else h:: (elimine t n)
;;

assert ((elimine [1;2;3;4;5;6] 3) = [1;2;4;5]);;

let rec ecreme (l: int list) : int list =
  match l with
    | [] -> []
    | [x] -> [x]
    | h::t -> h ::(ecreme (elimine t h))
;;

let crible (n:int) : int list =
  (ecreme (genere_list n))
;;

(*Ex 2.3*)

let degre (p: int list) : int =
  (List.length p) -1
;;

let rec value_of (x:int) (p:int list) : int =
  match p with
    | [] -> 0
    | [a] -> a
    | h::t -> h + x*(value_of (x) t)
;;

let is_root(x:int)(p:int list): bool =
  if ((value_of x p) == 0) then true
  else false;
;;

let upper_bound (p: int list) :int =
  (degre p) * (max_list(abs_list (rm_last p)))
;;

let rec filter_roots (l:int list)(p:int list) :int list =
  match l with 
    | [] -> []
    | [x] -> if ((is_root x p) == true) then [x]
      else []
    | h::t -> if ((is_root h p) == true) then h::(filter_roots t p)
      else (filter_roots t p)
;;

let roots_list (p: int list ): int list =
  (filter_roots(range_inter 0 (upper_bound p))p)
;;
