(* all the tests of the different functions of my files *)

(* don't forget to include the files morse.ml tp3.ml translation.ml when you run tests before importing this file *)

are_equal [1;2;3] [1;2;3];;
are_equal [1;4;6] [2;3];;

append ['h';'e'] ['y'];;

reverse [1;2;3;4];;

display ['h';'e';'y'];;

is_morse ['-';'.';' ';'/';'.'];;
is_morse ['o';'o'];;

letter_to_morse 'a';;
letter_to_morse 'A';;

word_to_morse ['s';'o';'s'];;

to_single_list [['t';'o'];['m'];['e']];;

display_2 [['t';'o'];['m'];['e']];;

sentence_to_morse [['t';'o'];['m'];['e']];;

sentence_to_single_list  [[['-']; ['-'; '-'; '-']]; [['-'; '-']; ['.']]];;

to_single_morse ['t';'o'];;

latin_sentence_to_single [['t';'o'];['m'];['e']];;

latin_to_morse "sos sos sos";;

morse_to_latin_string "...- .. ...- ./.-.. . .../.-.. .. ... - . .../";;

display_rhythm "... --- ... / ---- /";;


(* the function of STAGO 07 are the same, only the names are different *)
