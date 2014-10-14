#load "graphics.cma";;
open Graphics;;
open_graph "";;

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


(* 0.3 List List *)

let gen_board n =
  let rec aux d = function
    |0 -> [[]]
    |n -> (gen_list d) :: (aux d (n-1))
  in
  aux n n ;;



(* Random List List *)

let gen_rand_board n =
  let rec aux d = function
    |0 -> [[]]
    |n -> (gen_rand_list d) :: (aux d (n-1))
  in
  aux n n ;;



(*========================================================
			STAGE 01 - Tools
========================================================*)

(* 1.1 Count *)

let cell_count l =
  let rec aux n = function
    |[]  -> 0
    |0::[] -> n
    |1::[] -> n+1
    |1::t -> aux (n+1) t
    |0::t -> aux n t
    |_ -> failwith "wrong element inside the list"
  in 
  aux 0 l;; 


(* 1.2 Re-Count *)

let remaining l =
  let rec aux = function
    |[] -> 0
    |h::t -> cell_count h + aux t
  in
  aux l;;



(*=======================================================
			STAGE 02 - Display
=======================================================*)

(* 2.1 Square *)

let draw_square (x,y) size = 
  moveto x y;
  lineto (x+size) y;
  lineto (x+size) (y+size);
  lineto x (y+size);
  lineto x y;;


(* 2.2 Square - Again *)

let draw_fill_square (x,y) size color =
  set_color color;
  let finalX = x+size in 
  let rec aux x y = 
    moveto x y;
    match x with
    |x when x = finalX -> lineto x (y+size)
    |x -> lineto x (y+size); aux (x+1) y
  in
  aux x y;;


(* 2.3 Cell *)

let draw_cell (x,y) size cell = 
  match cell with
    |0 -> draw_square (x,y) size
    |1 -> draw_fill_square (x,y) size black
    |_ -> failwith "invalid cell";;


(* 2,4 Board *)

let rec draw_line line size (x,y) =
  match line with
    |[] -> ()
    |h::t -> draw_cell (x,y) size h; draw_line t size  (x+size,y);; 

let draw_board board size = 
  let (x,y) = (0,0) in
  let rec aux (x,y) = function
    |[] -> ()
    |h::t -> draw_line h size (x,y); aux (x,y+size) t
  in
  aux (x,y) board;;




(*======================================================
		STAGE 03 - Accessors
======================================================*)

(* 3.1 Cell *)

let get_element e l = 
  let rec aux n = function
    |[] -> failwith "out of bounds : not inside the list"
    |h::t -> 
      if n = e then h 
      else aux (n+1) t
  in aux 1 l;;

let get_cell (x,y) board = 
 let rec aux n = function
    |[] -> failwith "out of bound : not on the board"
    |h::t -> 
      if n = x then get_element y h
      else aux (n+1) t
  in
  aux 1 board;;

