#load "unix.cma";;
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


(* 3.2 Replace *)

let replace_element value element l = 
  let rec aux n = function
    |[] -> failwith "out of bounds : not inside Ze list"
    |h::t -> 
      if n = element then value::t
      else h::aux(n+1) t
  in aux 1 l;;

let replace_cell value (x,y) board =  
  let rec aux n = function
    |[] -> failwith "out of bounds : not on the board"
    |h::t -> 
      if n = x then (replace_element value y h)::t
      else h::aux(n+1) t
  in aux 1 board;;


(* 3.3 Seed Life *)

let my_list_length l =
  let rec aux n = function
    |[] -> n
    |h::t -> aux (n+1) t
  in aux 0 l;;

let seed_life board n = 
  let length = my_list_length board in
  if length*length - remaining board <= n then
    failwith "not enough space on the board"
  else
    let rec aux board n = 
      let x = (Random.int (length-1))+1 and y = (Random.int (length-1))+1 in  
      if (get_cell (x,y) board) = 1 then
        aux board n
      else
      match n with
        |0 -> board
        |n -> aux (replace_cell 1 (x,y) board) (n-1)
    in
    aux board n;;

(* 3.4 Neighborhood *)

(* 
let get_cell_neighborhood (x,y) board =
  let length = my_list_length board in
  if (x) > 1 && (y) > 1 then
    if (x+1) <= length && (y+1) <= length then
      (get_cell(x-1,y+1) board)::(get_cell(x,y+1)board)::(get_cell(x+1,y+1)board)::
      (get_cell(x-1,y)board)::(get_cell(x+1,y)board)::
      (get_cell(x-1,y-1)board)::(get_cell(x,y-1)board)::(get_cell(x+1,y-1)board)::[]
    else
      if (x+1) <= length then
        (get_cell(x-1,y)board)::(get_cell(x+1,y)board)::
        (get_cell(x-1,y-1)board)::(get_cell(x,y-1)board)::(get_cell(x+1,y-1)board)::[]
      else 
  else
    failwith "hehe";; 
*)


let get_cell_neighborhood (x,y) board =
  let length = my_list_length board -1 in
  if x > 0 && x <= length && y > 0 && y <= length then
    match (x,y) with
    (*the 4 corners *)
    | (x,y) when x = length && y = length -> (get_cell(x-1,y)board)::(get_cell(x-1,y-1)board)::(get_cell(x,y-1)board)::[]
    | (1,y) when y = length -> (get_cell(x+1,y)board)::(get_cell(x,y-1)board)::(get_cell(x+1,y-1)board)::[]
    | (x,1) when x = length -> (get_cell(x-1,y+1)board)::(get_cell(x,y+1)board)::(get_cell(x-1,y)board)::[]
    | (1,1) -> (get_cell(x,y+1)board)::(get_cell(x+1,y+1)board)::(get_cell(x+1,y)board)::[]
    (*the 4 sides *)
    | (1,y) when y <= length-1 -> (get_cell(x,y+1)board)::(get_cell(x+1,y+1)board)::
              (get_cell(x+1,y)board)::
              (get_cell(x,y-1)board)::(get_cell(x+1,y-1)board)::[]
    | (x,y) when x = length && y <= length-1 -> (get_cell(x-1,y+1)board)::(get_cell(x,y+1)board)::
              (get_cell(x-1,y)board)::
              (get_cell(x-1,y-1)board)::(get_cell(x,y-1)board)::[]
    | (x,1) when x <= length-1 -> (get_cell(x-1,y+1)board)::(get_cell(x,y+1)board)::(get_cell(x+1,y+1)board)::
              (get_cell(x-1,y)board)::(get_cell(x+1,y)board)::[]
    | (x,y) when x <= length-1 && y = length -> (get_cell(x-1,y)board)::(get_cell(x+1,y)board)::
              (get_cell(x-1,y-1)board)::(get_cell(x,y-1)board)::(get_cell(x+1,y-1)board)::[]
    | (x,y) -> (get_cell(x-1,y+1) board)::(get_cell(x,y+1)board)::(get_cell(x+1,y+1)board)::
              (get_cell(x-1,y)board)::(get_cell(x+1,y)board)::
              (get_cell(x-1,y-1)board)::(get_cell(x,y-1)board)::(get_cell(x+1,y-1)board)::[]
  else
    failwith "out of the board";;


(*===============================================
                  STAGE 04 - Game
===============================================*)

(* 4.1 Iteration *)

let iterate board =
  let length = my_list_length board -1 in
  let rec aux (x,y) board =
    match x with
      |x when x <= length ->begin
          match y with
            |y when y <= length -> begin
              if (get_cell(x,y) board) = 1 then
                match cell_count(get_cell_neighborhood (x,y) board) with
                |n when n < 2 -> aux(x,y+1) (replace_cell 0 (x,y) board) (*dies*)
                |n when n > 3 -> aux(x,y+1)(replace_cell 0 (x,y) board) (*dies*)
                |_ -> aux(x,y+1) board (*lives/stays alive*)
              else  
                (match cell_count(get_cell_neighborhood (x,y) board) with
                |3 -> aux(x,y+1)(replace_cell 1 (x,y) board) (*lives / comes to live *)
                |_ -> aux(x,y+1) board)
            end
            |_ -> aux(x+1,1) board
      end
      |_ -> board
  in
  aux (1,1) board;;


  (* 4.2 Play *)

let rec play board =
  Unix.sleep(1);
  clear_graph();
  match remaining(board) with
  | 0 -> draw_board board 20
  | _ -> draw_board board 20; play (iterate board);;


(*==================================================
                  STAGE 05 - Bonus
==================================================*)

(* 5.1 Health Points *)

(*defining the colors corresponding to the differents levels of life *)
let _10 = rgb 0 191 133 and
_9 = rgb 0 247 107 and
_8 = rgb 93 250 61 and
_7 = rgb 168 250 61 and
_6 = rgb 218 251 84 and
_5 = rgb 247 247 7 and
_4 = rgb 242 202 110 and 
_3 = rgb 236 118 0 and
_2 = rgb 247 74 0 and
_1 = rgb 147 33 33 and
_0 = rgb 42 42 42;;

(** 5.1.1 Rules **)
(** 5.1.2 Evolving **)

let gen_rand_list_bonus n = 
  let rec aux = function
    |0 -> []
    |n -> (Random.int(2) * 10):: aux(n-1)
  in
  aux n;;

let gen_rand_board_bonus n =
  let rec aux d = function
    |0 -> [[]]
    |n -> (gen_rand_list d) :: (aux d (n-1))
  in
  aux n n ;;

let cell_count_bonus l =
  let rec aux n = function
    |[]  -> 0
    |h::[] when h <= 1 -> n
    |h::[] when h <= 10 && h > 1 -> n+1
    |h::t when h <= 10 && h > 1 -> aux (n+1) t
    |h::t when h <= 1 -> aux n t
    |_ -> failwith "wrong element inside the list"
  in 
  aux 0 l;; 

let remaining_bonus l =
  let rec aux = function
    |[] -> 0
    |h::t -> cell_count_bonus h + aux t
  in
  aux l;;

let draw_cell_bonus (x,y) size cell = 
  match cell with
    |n when n <= 0 -> draw_square (x,y) size
    |1 -> draw_fill_square (x,y) size _1
    |2 -> draw_fill_square (x,y) size _2
    |3 -> draw_fill_square (x,y) size _3
    |4 -> draw_fill_square (x,y) size _4
    |5 -> draw_fill_square (x,y) size _5
    |6 -> draw_fill_square (x,y) size _6
    |7 -> draw_fill_square (x,y) size _7
    |8 -> draw_fill_square (x,y) size _8
    |9 -> draw_fill_square (x,y) size _9
    |10 -> draw_fill_square (x,y) size _10
    |_ -> draw_fill_square (x,y) size _10(*failwith "invalid cell"*);;

let rec draw_line_bonus line size (x,y) =
  match line with
    |[] -> ()
    |h::t -> draw_cell_bonus (x,y) size h; draw_line_bonus t size  (x+size,y);; 

let draw_board_bonus board size = 
  let (x,y) = (0,0) in
  let rec aux (x,y) = function
    |[] -> ()
    |h::t -> draw_line_bonus h size (x,y); aux (x,y+size) t
  in
  aux (x,y) board;;

let seed_life_bonus board n = 
  let length = my_list_length board in
  if length*length - remaining_bonus board <= n then
    failwith "not enough space on the board"
  else
    let rec aux board n = 
      let x = (Random.int (length-1))+1 and y = (Random.int (length-1))+1 in  
      if (get_cell (x,y) board) <> 0 then
        aux board n
      else
      match n with
        |0 -> board
        |n -> aux (replace_cell 10 (x,y) board) (n-1)
    in
    aux board n;;

let iterate_bonus board =
  let length = my_list_length board -1 in
  let rec aux (x,y) board =
    match x with
      |x when x <= length ->begin
          match y with
            |y when y <= length -> begin
              if (get_cell(x,y) board) = 1 then
                match cell_count(get_cell_neighborhood (x,y) board) with
                |n when n < 2 -> aux(x,y+1) (replace_cell (get_cell(x,y)board -2) (x,y) board) (*dies*)
                |n when n = 3 -> aux(x,y+1)(replace_cell (get_cell(x,y)board +1) (x,y) board) (*dies*)
                |n when n > 3 -> aux(x,y+1)(replace_cell (get_cell(x,y)board -5) (x,y) board) (*dies*)
                |_ -> aux(x,y+1) board (*lives/stays alive*)
              else  
                (match cell_count(get_cell_neighborhood (x,y) board) with
                |3 -> aux(x,y+1)(replace_cell 1 (x,y) board) (*lives / comes to live *)
                |_ -> aux(x,y+1) board)
            end
            |_ -> aux(x+1,1) board
      end
      |_ -> board
  in
  aux (1,1) board;;

  let rec play_bonus board =
  Unix.sleep(1);
  clear_graph();
  match remaining_bonus(board) with
  | 0 -> draw_board board 20
  | _ -> draw_board board 20; play (iterate board);;