
(* 0.2_ Concatenation *)

let append l1 l2 = 
	let rec aux = function
		|([],[]) -> []
		|(l,[])|([],l) -> l
		|(h::[],l) -> h::l
		|(h::t,l) -> h::aux (t,l)
	in
	aux (l1,l2);;


(* control of empty list *)

let is_empty = function
	|[] -> true
	|_ -> false;;
