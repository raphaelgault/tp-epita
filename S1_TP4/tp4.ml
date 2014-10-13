open Random;;
self_init ();;

(*=====================================================
			STAGE 0 - Generator
=====================================================*)

(* 0.1 List *)

let gen_list n =
  let rec aux = function
    |0 -> []
    |n -> 0::aux (n-1)
  in 
  aux n;;



(* 0.2 Random List *)

let gen_rand_list n = 
  let rec aux = function
    |0 -> []
    |n -> Random.int(2) :: aux(n-1)
  in
  aux n;;
