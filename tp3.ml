(*====================================
		STAGE 01 
====================================*)


(* 0.1_ Equality *)

let are_equal l1 l2 = 
	let rec aux = function
		|([],[]) -> true
		|(h1::t1,h2::t2) -> 
			begin 
			match (h1,h2) with 
				|(x,y) when x = y -> aux (t1,t2);
				|_ -> false
				end
		|(_,_) -> false	
	in
	aux (l1,l2);;





(* 0.2_ Concatenation *)

let append l1 l2 = 
	let rec aux = function
		|([],[]) -> []
		|(l,[])|([],l) -> l
		|(h::[],l) -> h::l
		|(h::t,l) -> h::aux (t,l)
	in
	aux (l1,l2);;
	



(* 0.3_ Reverse *)

let reverse l = 
	let rec aux a = function
		|[] -> a
		|h::t -> aux (h::a) t
	in
	aux [] l;;




(* 0.4_ Display *)
(* Works Only with char list -as specified in the subject- *)
let display l = 
	print_char('[');
	let rec aux l = 	
	match l with 
		|[] -> print_char(']')
		|h::[]->print_char(h);print_char(']')
		|h::t -> print_char(h);print_char(';');aux t
	in
	aux l;;





