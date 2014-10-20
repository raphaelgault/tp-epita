(*=======================================================
                 LEVEL 0 - Must do
=======================================================*)

(* auxiliary *)
let rec append l1 l2 =
  match l1 with
    |[] -> l2
    |h::t -> h::(append t l2);;

(* 0.1 Length *)

let length l =
  let rec aux n = function
    |[] -> n
    |h::t -> aux (n+1) t
  in 
  aux 0 l;;


(* 0.2 Flatten *)

let rec flatten  = function
  |[] -> []
  |h::t -> append h (flatten t);;

