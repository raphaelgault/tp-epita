#use "auxiliary_operation.ml";;


(*====================================
		STAGE 07 
====================================*)


let encode_letter  c = 
		match c with
		| 'A'| 'a'-> _A
		| 'B'| 'b'-> _B
		| 'C'| 'c'-> _C
		| 'D'| 'd'-> _D
		| 'E'| 'e'-> _E
		| 'F'| 'f'-> _F
		| 'G'| 'g'-> _G
		| 'H'| 'h'-> _H
		| 'I'| 'i'-> _I
		| 'J'| 'j'-> _J
		| 'K'| 'k'-> _K
		| 'L'| 'l'-> _L
		| 'M'| 'm'-> _M
		| 'N'| 'n'-> _N
		| 'O'| 'o'-> _O
		| 'P'| 'p'-> _P
		| 'Q'| 'q'-> _Q
		| 'R'| 'r'-> _R
		| 'S'| 's'-> _S
		| 'T'| 't'-> _T
		| 'U'| 'u'-> _U
		| 'V'| 'v'-> _V
		| 'W'| 'w'-> _W
		| 'X'| 'x'-> _X
		| 'Y'| 'y'-> _Y
		| 'Z'| 'z'-> _Z
		| '0' -> _0
		| '1' -> _1
		| '2' -> _2
		| '3' -> _3
		| '4' -> _4
		| '5' -> _5
		| '6' -> _6 
		| '7' -> _7 
		| '8' -> _8 
		| '9' -> _9
		|_ -> failwith "the char has to be a letter or a digit";;
		


(* 2.1_ Conversion *)

let rec encode_word = function 
	|[] -> []
	|h::t -> append [encode_letter h] (encode_word t);;	


(* 3.1_ Conversion (again) *)

let rec encode_sentence = function 
	|[] -> []
	|h::t -> append [encode_word h] (encode_sentence t);;


(* Encode Me *)


let string_to_single_list s = 
	let length = String.length s in
	let rec aux n = 
		if s.[n] <> ' ' then
			match n with
			|n when n = length-1 -> append (encode_letter s.[n]) ['/'] 
			|_ -> append (append (encode_letter s.[n]) [' ']) (aux(n+1))
		else
			match n with
			|n when n = length-1 -> ['/']
			|_ -> append ['/'] (aux (n+1))
	in
	aux 0;;

let list_to_string l = 
	let rec aux = function
		|[] -> ""
		|h::t -> String.make 1 h ^  aux t
	in
	aux l;;

let encode_latin s =
	list_to_string (string_to_single_list s);;


(* Decode Me *)

let decode_letter l =
	match l  with
                | l when l = _A -> 'A'
                | l when l = _B -> 'B'
                | l when l = _C -> 'C'
                | l when l = _D -> 'D'
                | l when l = _E -> 'E'
                | l when l = _F -> 'F'
                | l when l = _G -> 'G'
                | l when l = _H -> 'H'
                | l when l = _I -> 'I'
                | l when l = _J -> 'J'
                | l when l = _K -> 'K'
                | l when l = _L -> 'L'
                | l when l = _M -> 'M'
                | l when l = _N -> 'N'
                | l when l = _O -> 'O'
                | l when l = _P -> 'P'
                | l when l = _Q -> 'Q'
                | l when l = _R -> 'R'
                | l when l = _S -> 'S'
                | l when l = _T -> 'T'
                | l when l = _U -> 'U'
                | l when l = _V -> 'V'
                | l when l = _W -> 'W'
                | l when l = _X -> 'X'
                | l when l = _Y -> 'Y'
                | l when l = _Z -> 'Z'
                | l when l = _0 -> '0'
                | l when l = _1 -> '1'
                | l when l = _2 -> '2'
                | l when l = _3 -> '3'
                | l when l = _4 -> '4'
                | l when l = _5 -> '5'
                | l when l = _6 -> '6'
                | l when l = _7 -> '7'
                | l when l = _8 -> '8'
                | l when l = _9 -> '9' 
                |_ -> failwith "error";;



let decode_word l = 
	let rec aux c = function
			|[] -> ""
			|' '::t -> String.make 1 (decode_letter c) ^ aux [] t
			|h::t -> aux (append c [h]) t
	in
	aux [] l;;

let decode_string s = 
	let length = String.length s in
	let rec aux  w n =
		if n < (length) then
		begin
			match s.[n] with
				|'/' -> (decode_word w) ^ " " ^ aux [] (n+1) (* transformer on lettre *)
				|c -> aux (append w [c]) (n+1)
		end
		else
			begin
			display w;decode_word w(* transformer en lettre *)
			end
	in
	aux [] 0;;


  
