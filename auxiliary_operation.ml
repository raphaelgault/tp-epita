
(* 0.2_ Concatenation *)

let append l1 l2 = 
	let rec aux = function
		|([],[]) -> []
		|(l,[])|([],l) -> l
		|(h::[],l) -> h::l
		|(h::t,l) -> h::aux (t,l)
	in
	aux (l1,l2);;
	
let display l = 
        print_char('[');
        let rec aux l =         
        match l with 
                |[] -> print_char(']')
                |h::[]->print_char(h);print_char(']')
                |h::t -> print_char(h);print_char(';');aux t
        in
        aux l;;

