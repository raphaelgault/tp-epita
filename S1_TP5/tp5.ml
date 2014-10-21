(* auxiliary *)

let append l1 l2 = 
  let rec aux = function
    |[] -> l2
    |h::t -> h::(aux t)
  in 
  aux l1;;

let rec already_in a = function
  |[] -> false
  |h::t when h = a -> true
  |h::t -> already_in a t;;

let  my_list_length l =
  let rec aux acc = function
    |[] -> acc
    |h::t -> aux (acc+1) t
  in
  aux 0 l;;

let rec my_power x = function
    |0 -> 1
    |n when n mod 2 = 0-> my_power (x*x) (n/2) 
    |n -> x * my_power (x*x) (n/2);;


(*==========================================
            STAGE 00 - Must do
==========================================*)

(* Sum Types *)

type 'a bundle =
  |Empty 
  |Item  of 'a * 'a bundle;;

(* 0.1 Empty *)

let empty_bundle () = Empty;;

(* 0.2 Is Empty ? *)

let is_empty b =
  b = Empty;;


(* 0.3 Constructor *)

let cons bund el =
  Item(el,bund);;

(* 0.4 The First *)

let head = function
  |Empty -> failwith "Head failed : empty bundle"
  |Item(a,bundle) -> a;; 

(* 0.5 The Others *)

let tail = function
  |Empty -> failwith "Tail failed : empty bundle"
  |Item(a,bundle) -> bundle;;




(*===========================================
             STAGE 01 - Boolean Expression
===========================================*)

(* Sum types *)

type boolean = 
  | True | False
  | Var of string
  | Not of boolean
  | And of boolean * boolean
  | Or of boolean * boolean;;



(* 1.1 Value ? *)

let rec value s = function
  |[] -> failwith ("Unbound Var :"^s)
  |(a,bundle)::t -> 
    if a = s then
     bundle
    else value s t;;

(* 1.2 Extraction *)

(* using the function already_in defined at the top of the file *)
let extract b =
  let rec aux l = function
    |True | False -> l 
    |Var s -> if (already_in s l) then l else s::l
    |Not b -> aux l b
    |And (b1,b2) | Or (b1,b2) -> aux (aux l b1) b2
  in
  aux [] b;;

(* 1.3 Generate *)

(*
let rec generate = function
  |[] ->[]
  |h::t -> (h,True)::(h,False)::(generate t);;
i*)

(*step 1 : count number of given identifiers -> my_list_length *)
(*step 2 : compute number of combination -> power of 2 *)
(*step 3 : fix the first id for 2^(n-1) combination and the second id for
 * 2^(n-2) ... n id not fixed -> move at every combination *)
(*step 4 : each combination is stored inside a list which is in a list too *)

let rec distrib a = function
  |[] -> []
  |h::t -> (a::h)::distrib a t

let rec generate = function
  |[] -> []
  |h::[] -> [(h,True)]::[(h,False)]::[]
  |h1::t -> distrib (h1,True) (generate t)@
    distrib (h1,False) (generate t);;



(*
let rec generate = function
    |[] -> []
    |h::[] -> combine h
    |h1::h2::t ->
      begin 
        match (combine h1,combine h2) with 
          |(a1::a2::[],b1::b2::[]) -> [b1::a1]::[b1::[a2]]::[b2::[a1]]::[b2::[a2]]::[]
          |_ -> failwith "booh" 
      end;;
     

*)
