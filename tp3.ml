#use "morse.ml";;
#load "unix.cma";;
#directory "+threads";;
#load "threads.cma";;
#load "graphics.cma";;
open Graphics;;
open_graph "";;


(* addon : control of the content of a list *)
let is_empty = function
	|[] -> true
	|_ -> false ;;




(*====================================
		STAGE 00 
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





(*=========================================
		STAGE 01
=========================================*)


(* 1.1_ Validity *)

let rec is_morse = function
		|[] -> true
		|h::t -> match h with 
				|'.'|'-'|' '|'/' ->is_morse t
				|_ -> false;;





(* 1.2_ Conversion *)

(* |are_equal l _A ->
		|are_equal l _B ->
		|are_equal l _C ->
		|are_equal l _D ->
		|are_equal l _E ->
		|are_equal l _F ->
		|are_equal l _G ->
		|are_equal l _H ->
		|are_equal l _I ->
		|are_equal l _J ->
		|are_equal l _K ->
		|are_equal l _L ->
		|are_equal l _M ->
		|are_equal l _N ->
		|are_equal l _O ->
		|are_equal l _P ->
		|are_equal l _Q ->
		|are_equal l _R ->
		|are_equal l _S ->
		|are_equal l _T ->
		|are_equal l _U ->
		|are_equal l _V ->
		|are_equal l _W ->
		|are_equal l _X ->
		|are_equal l _Y ->
		|are_equal l _Z ->
		|are_equal l _0 ->
		|are_equal l _1 ->
		|are_equal l _2 ->
		|are_equal l _3 ->
		|are_equal l _4 ->
		|are_equal l _5 ->
		|are_equal l _6 -> 
		|are_equal l _7 -> 
		|are_equal l _8 -> 
		|are_equal l _9 -> *)

let letter_to_morse c = 
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
		


(*======================================
		STAGE 02
======================================*)


(* 2.1_ Conversion *)

let rec word_to_morse = function 
	|[] -> [] 
	|h::t -> append [letter_to_morse h] (word_to_morse t);;	






(* 2.2_ Another way to see things *)

let rec to_single_list = function 
	|[] -> []
	|h::[] -> h
	|h::t -> append (append h [' ']) (to_single_list t);; 






(* 2.3_ Printing *)

let display_2 l = 
	print_char('[');
	let rec aux = function
		|[] -> print_char (']')
		|h::[] -> display h; print_char(']')
		|h::t -> display h ;print_char(';'); aux t
	in
	aux l;;
	






(*=========================================
		STAGE 03
=========================================*)


(* 3.1_ Conversion (again) *)

let rec sentence_to_morse = function 
	|[] -> []
	|h::t -> append [word_to_morse h] (sentence_to_morse t);;




(* 3.2_ char list list *)

let rec sentence_to_single_list = function
	|[] -> []
	|h::t -> append (append (to_single_list h) ['/']) (sentence_to_single_list t);;




(* 3.3_ Non Stop *)

let to_single_morse l = 
	to_single_list (word_to_morse l);;


let latin_sentence_to_single l =
	sentence_to_single_list (sentence_to_morse l);;



(*=========================================
		STAGE 04
=========================================*)

(* Encode Me *)


let string_to_single_list s = 
	let length = String.length s in
	let rec aux n = 
		if s.[n] <> ' ' then
			match n with
			|n when n = length-1 -> append (letter_to_morse s.[n]) ['/'] 
			|_ -> append (append (letter_to_morse s.[n]) [' ']) (aux(n+1))
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

let latin_to_morse s =
	list_to_string (string_to_single_list s);;



(*==========================================
		STAGE 05
==========================================*)


(* Decode Me *)

let morse_to_letter l =
	if is_morse l then
	begin
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
                |_ -> failwith "error"
        end
        else
        failwith "not morse code";;


(*
let string_to_morse_list s = 
	let length = String.length s and l = [[]] in
	let rec aux n =
		if s.[n] <> '/' then
		begin
			if s.[n] <> ' ' then
				match n with
				|n when n = length-1 -> [s.[n]]
				|_ -> append [s.[n] aux(n-1)
			else
			match n with
				|n when n = length-1 -> []
				|_ 
		end
		else;; *)





let word_to_latin l = 
	let rec aux c = function
                         |[] when is_empty c -> ""
                         |[] -> String.make 1 (morse_to_letter c)
                         |' '::t -> String.make 1 (morse_to_letter c) ^ aux [] t
                         |h::t -> aux (append c [h]) t
         in
         aux [] l;;

let morse_to_latin_string s = 
	let length = String.length s in
	let rec aux  w n =
		if n < length-1 then
		begin
			match s.[n] with
				|'/' -> (word_to_latin w) ^ " " ^ aux [] (n+1) (* transformer on lettre *)
				|c -> aux (append w [c]) (n+1)
		end
		else
			word_to_latin w(* transformer en lettre *)
	in
	aux [] 0;;


(*===========================================
		STAGE 06
===========================================*)

			

let display_rhythm s = 
	let length = String.length s in
	let rec aux n = 
		if n < length then
			begin
			print_char s.[n];
			flush stdout;
			(	match s.[n] with
				|'.' -> Thread.delay dits;sound 440 (int_of_float(dits *. 1000.));Thread.delay dits
				|'-' -> Thread.delay dahs;sound 440 (int_of_float(dahs *. 1000.));Thread.delay dits
				|' ' -> Thread.delay dahs;Thread.delay dits
				|'/' -> Thread.delay gap;Thread.delay dits
			);	
			aux (n+1);
			end
	in 
	aux 0;; 


display_rhythm "... --- ... / ---- /";;




		
			
  
