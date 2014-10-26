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


let rec convert_10_to_2 x = 
  match (x mod 2) with
    |0 when x/2 = 0 -> [] (*end of conversion *)
    |r -> append(convert_10_to_2 (x/2)) [r];;






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


(* using distributivity *)
(*
let rec distrib a = function
  |[] -> []
  |h::t -> (a::h)::distrib a t

let rec generate = function
  |[] -> []
  |h::[] -> [(h,True)]::[(h,False)]::[]
  |h1::t -> distrib (h1,True) (generate t)@
    distrib (h1,False) (generate t);;
*)



(* using a base converter function defined on the top *)
let rec create_list el = function
  |0 -> []
  |n -> el::(create_list el (n-1));;


let binary_to_uniform l length =
  let diff_length = length - my_list_length l in
    match diff_length with
      |0 -> l
      |d when d > 0 -> append (create_list 0 d) l
      |_ -> failwith "length given is too short";;


let rec binary_to_boolean ids bin = 
  match (ids,bin) with
    |([],[]) -> []
    |(h::t,0::l) -> (h,False)::(binary_to_boolean t l)
    |(h::t,1::l) -> (h,True)::(binary_to_boolean t l)
    |_ -> failwith "invalid lists : not corresponding";;

let binary_to_uniformed_boolean ids l length =
  binary_to_boolean ids (binary_to_uniform l length);;


let generate l =
  let length = my_list_length l in
  let combination = my_power 2 length in
  let rec aux = function
    |0 -> [binary_to_uniformed_boolean l [0] length]
    |n -> (binary_to_uniformed_boolean l (convert_10_to_2 n) length)::aux (n-1)
  in 
  aux (combination - 1)




 
(* 1.4 Evaluation *)

let eval expr l =
  match expr with
  |Not b -> begin
    match l with
      |(a,True)::[] -> False
      |(a,False)::[] -> True
      |_ -> failwith "invalid list of boolean"
    end
  |And (b1,b2) -> begin 
    match l with
      |(a,True)::(b,True)::[] -> True
      |_ -> False
    end
  |Or (b1,b2) -> begin
    match l with
      |(_,True)::h::[] | h::(_,True)::[] -> True
      |_ -> False
    end
  |_ -> failwith "invalid boolean expression";;

let evaluate expr =
  let ids = generate(extract expr) in
  let rec aux = function
    |[] -> []
    |h::t -> (h,eval expr h)::aux t
  in
  aux ids;;





(* 1.5 Display *)

let print_bool = function
  |True -> print_string("T")
  |False -> print_string("F")
  |_ -> failwith "wrong boolean value";;


let rec print_ids = function
  |h::[] -> print_string ((fst h)^" ");print_newline()
  |h::t -> print_string ((fst h)^" ");print_ids t
  |_ -> ();;

let print_ids_inline l =
   match l with
    |h::t -> print_ids (fst h)
    |[] -> ();;
   (* |_ -> failwith "wrong list";*)

let rec print_line = function
  |h::[] -> print_bool(snd h);print_string("  ")
  |h::t -> print_bool(snd h);print_string(" ");print_line t
  |_ -> failwith "booh";;

let print_result = function
  |(l,b) -> print_bool(b);print_newline();;

let display l = 
  print_ids_inline l;
  let rec aux = function
    |h::[] -> print_line(fst h);print_result h
    |h::t -> print_line(fst h);print_result h;aux t
    |_ ->()
  in
  aux l;;




(*=========================================================
                  STAGE 02 - Parsing
=========================================================*)


(* 2.1 Parse *) 

let parse s =
  let length = String.length s in
  let rec aux n output operators =
    if n < length then
      match s.[n] with
        |'!' -> aux (n+1) output (cons operators s.[n])
        |'&' -> if is_empty operators then
          aux (n+1) output (cons operators s.[n])
          else
            (match head operators with
              |'!' -> aux (n+1) (cons output '!') (cons (tail operators) '!')
              |_ -> aux (n+1) output (cons operators (head operators)))
        |'|' -> if is_empty operators then
          aux (n+1) output (cons operators s.[n])
          else
          aux (n+1) (cons output(head operators))(cons(tail operators) s.[n])
        |_ -> aux (n+1) (cons output s.[n]) operators
    else
      if is_empty operators then
        output
      else
        aux n (cons output (head operators)) (tail operators)
  in
  aux 0 (empty_bundle()) (empty_bundle());;






(* 2.2 Builder *)

let extract_bundle bund =
  let rec aux l = function
    |Item(a,Empty) -> a::l
    |Item(a,bundle) -> (aux (a::l) bundle)
    |Empty -> failwith "extraction failed : empty bundle"
  in 
  aux [] bund;;

let rec foot = function
  |Item(a,Empty) -> a
  |Item(a,bundle) -> foot bundle
  |Empty -> failwith "Foot failed : Empty bundle";;

let mid_bundle bund = 
  let l = match extract_bundle bund with
      |h::t -> t
      |_ -> failwith "empty list"
  in
  let rec aux mid = function
    |h::[] -> mid
    |h::t -> aux (cons mid h) t
    |_ -> failwith "only one element in the list"
  in
  aux (empty_bundle ()) l;;

let get_second bund =
  head (tail bund);;


let rec builder bund = 
  if is_empty bund then
    True
  else
  match head bund with
    |'|' -> Or(builder (mid_bundle bund),Var (String.make 1 (foot bund)))
    |'&' -> And(builder (mid_bundle bund),Var (String.make 1 (foot bund)))
    |'!' -> Not(builder (tail bund))
    |el -> Var (String.make 1 el);;



let reverse bundle =
  let rec aux l = function
    |Item(a,Empty) -> a::l
    |Item(a,bundle) -> aux (a::l) bundle
    |Empty -> l
  in
  aux [] bundle;; 

let builder bundle =
  let expr = reverse bundle in
  let rec aux stack = function
    |'|'::t -> (match stack with
      |h1::h2::l -> aux (Or(h1,h2)::l) t
      |_ -> failwith "blabla")
    |'&'::t ->(match stack with
      |h1::h2::l -> aux (And(h1,h2)::l) t
      |_ -> failwith "blob")
    |'!'::t -> (match stack with
      |h1::l -> aux ((Not(h1))::l) t
      |_ -> failwith "42")
    |h::t -> aux ((Var (String.make 1 h))::stack) t
    |[] -> (match stack with
      |h::[] -> h
      |_ -> failwith "bab")
  in 
  aux [] expr;;





(* 2.3 Truth table *)

let truth_table s =
  display (evaluate (builder (parse s)));;
