(*Ex 1.1*)
let rec sum_chiffres (n:int) : int =
  if (n == 0) then 0
  else (n mod 10) + (sum_chiffres (n/10));;

assert((sum_chiffres 1234)  = 10);; 

let rec nb_chiffres(n:int) : int =
  if (n==0) then 0
  else 1 + (nb_chiffres (n/10));;

assert((nb_chiffres 125) = 3);;
assert((nb_chiffres 125465) = 6);;
assert((nb_chiffres 1256) = 4);;

(*Ex 1.2*)

let rec less_divider (i:int) (n:int) : int =
  if (i == 0) then 0
  else
    if (i == n) then 0
    else 
      if (n mod i == 0) then i
      else (less_divider (i+1) n);;

assert((less_divider 2 19) = 0);;
assert((less_divider 7 21) = 7);;
assert((less_divider 5 21) = 7);;
assert((less_divider 9 21) = 0);;

let prime (n:int) : bool =
  if (n<=1) then false
  else
    if (less_divider 2 n) == 0 then true
    else false;;

assert((prime 1) = false);;
assert((prime 21) = false);;
assert((prime 19) = true);;

let rec next_prime(n:int):int =
    if ((prime n) == true) then n 
    else (next_prime (n+1));;

assert((next_prime 0) = 2);;
assert((next_prime 15) = 17);;

let rec next_prime_sup(n:int):int =
  if ((prime (n+1)) == true) then n+1 
  else (next_prime (n+2));;


let rec nth_prime (n:int) :int =
  if (n == 0) then (next_prime_sup 0)
  else (next_prime_sup(nth_prime (n-1)));;

assert ((nth_prime 0) = 2) ;;
assert ((nth_prime 1) = 3) ;;
assert ((nth_prime 2) = 5) ;;
assert ((nth_prime 3) = 7) ;;
assert ((nth_prime 12) = 41) ;;

(*Ex 1.3*)

let y = 6;;
let foo(x:int):int =
  x + y;;
assert ((foo 4) = 10);;
let y = 18;;
assert ((foo 4) = 10);;

(*
Q.4 (foo 4) vaut 10 dans E1 et 10 dans E2
Q.6 Le corps d'une fonction OCaml est evalue dans l'environnement E1
Q.8 Le corps d'une fonction Python est evalue dans l'environnement E2
*)