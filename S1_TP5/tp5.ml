(* auxiliary *)

let append l1 l2 = 
  let rec aux = function
    |[] -> l2
    |h::t -> h::(aux t)
  in 
  aux l1;;

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
  |[] -> failwith "unbound value"
  |(a,bundle)::t -> 
    if a = s then
     bundle
    else value s t;;

(* 1.2 Extraction *)

let rec extract = function
  |True | False -> []
  |Var s -> [s]
  |Not b -> extract b
  |And (b1,b2) | Or (b1,b2) -> append (extract b1) (extract b2);;

(* 1.3 Generate *)

(*
let rec generate = function
  |[] ->[]
  |h::t -> (h,True)::(h,False)::(generate t);;
i*)
