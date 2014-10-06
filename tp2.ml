#!/usr/bin/env ocaml

open Complex;;
open Random;;


#load "graphics.cma";;
open Graphics;;
(* open_graph "";; *)


Random.self_init ();;


let draw_line (x, y) (j, k) =
	moveto (x) (y);
	lineto (j) (k)
;;


(*
      ░░░░░░░░░▄░░░░░░░░░░░░░░░▄
      ░░░░░░░░▌▒█░░░░░░░░░░░▄▀▒▌
      ░░░░░░░░▌▒▒█░░░░░░░░▄▀▒▒▒▐
      ░░░░░░░▐▄▀▒▒▀▀▀▀▄▄▄▀▒▒▒▒▒▐ SUCH SPEED
      ░░░░░▄▄▀▒░▒▒▒▒▒▒▒▒▒█▒▒▄█▒▐
      ░░░▄▀▒▒▒░░░▒▒▒░░░▒▒▒▀██▀▒▌
      ░░▐▒▒▒▄▄▒▒▒▒░░░▒▒▒▒▒▒▒▀▄▒▒▌
  WOW ░░▌░░▌█▀▒▒▒▒▒▄▀█▄▒▒▒▒▒▒▒█▒▐
      ░▐░░░▒▒▒▒▒▒▒▒▌██▀▒▒░░░▒▒▒▀▄▌
      ░▌░▒▄██▄▒▒▒▒▒▒▒▒▒░░░░░░▒▒▒▒▌
      ▌▒▀▐▄█▄█▌▄░▀▒▒░░░░░░░░░░▒▒▒▐
      ▐▒▒▐▀▐▀▒░▄▄▒▄▒▒▒▒▒▒░▒░▒░▒▒▒▒▌
      ▐▒▒▒▀▀▄▄▒▒▒▄▒▒▒▒▒▒▒▒░▒░▒░▒▒▐
AMAZE ░▌▒▒▒▒▒▒▀▀▀▒▒▒▒▒▒░▒░▒░▒░▒▒▒▌
      ░▐▒▒▒▒▒▒▒▒▒▒▒▒▒▒░▒░▒░▒▒▄▒▒▐
      ░░▀▄▒▒▒▒▒▒▒▒▒▒▒░▒░▒░▒▄▒▒▒▒▌ SUCH LIST
      ░░░░▀▄▒▒▒▒▒▒▒▒▒▒▄▄▄▀▒▒▒▒▄▀
      ░░░░░░▀▄▄▄▄▄▄▀▀▀▒▒▒▒▒▄▄▀
      ░░░░░░░░░▒▒▒▒▒▒▒▒▒▒▀▀
*)
let mountain_list n =
	let rec join_points = function
		| a::b::p ->
			draw_line a b;
			join_points (b::p)
		| _ ->
			()
	in
	let rec add_points = function
		| (x, y)::(j, k)::p ->
			let u = (x + j) / 2
			and v = (y + k) / 2 + Random.int (n * 10)
			in
			(x, y)::(u, v)::add_points ((j, k)::p)
		| p ->
			p
	in
	let rec mount p = function
		| 0 ->
			join_points p
		| n ->
			mount (add_points p) (n - 1)
	in
	mount [(100, 500); (800, 500)] n
;;


(* Clean recursive version *)
let mountain n =
	let rec mount (x, y) (p, q) = function
		| 0 ->
			draw_line (x, y) (p, q)
		| n ->
			let u = (x + p) / 2
			and v = (y + q) / 2 + Random.int (n * 10)
			in
			mount (x, y) (u, v) (n - 1);
			mount (u, v) (p, q) (n - 1)
	in
	mount (100, 500) (800, 500) n
;;


let dragon n =
	let rec drag (x, y) (p, q) = function
		| 0 ->
			draw_line (x, y) (p, q)
		| n ->
			let u = (x + p) / 2 + (q - y) / 2
			and v = (y + q) / 2 - (p - x) / 2
			in
			drag (x, y) (u, v) (n - 1);
			drag (p, q) (u, v) (n - 1)
	in
	drag (300, 500) (800, 500) n
;;


(*
    c

 ac    bc

a   ab   b
*)
let sierpinski n =
	let mid (aX, aY) (bX, bY) =
		((aX + bX) / 2, (aY + bY) / 2)
	in
	let rec sier a b c = function
		| 0 ->
			draw_line a b;
			draw_line b c;
			draw_line c a
		| n ->
			let ab = mid a b
			and bc = mid b c
			and ca = mid c a
			in
			sier  a ab ca (n - 1);
			sier ab  b bc (n - 1);
			sier ca bc  c (n - 1)
	in
	sier (50, 50) (850, 50) (450, 743) n
;;


(* Draw upside down triangles, greatly improves speed *)
let sierpinski_rev n =
	let mid (aX, aY) (bX, bY) =
		((aX + bX) / 2, (aY + bY) / 2)
	in
	let rec sier a b c = function
		| 0 ->
			()
		| n ->
			let ab = mid a b
			and bc = mid b c
			and ca = mid c a
			in
			draw_line ab bc;
			draw_line bc ca;
			draw_line ca ab;
			sier  a ab ca (n - 1);
			sier ab  b bc (n - 1);
			sier ca bc  c (n - 1)
	in
	let a = (50, 50)
	and b = (850, 50)
	and c = (450, 743)
	in
	(* Draw outside triangle *)
(* 	draw_line a b;
	draw_line b c;
	draw_line c a; *)
	sier a b c n
;;


let third_vect (aX, aY) (bX, bY) =
	let vX = (bX - aX) / 3
	and vY = (bY - aY) / 3
	in
	function (x, y) ->
		((x + vX), (y + vY))
;;


(*
 d   dc   cd  c

da   d'   c'  cb

ad   a'   b'  bc

 a   ab   ba  b
*)
let vicsek n =
	let rec vic a b c d = function
		| 0 ->
			draw_line a b;
			draw_line b c;
			draw_line c d;
			draw_line d a
		| n ->
			let vR = third_vect a b
			and vU = third_vect b c
			in
			let ab = vR a
			and bc = vU b
			and dc = vR d
			and ad = vU a
			in
			let ba = vR ab
			and cb = vU bc
			and cd = vR dc
			and da = vU ad
			in
			let a' = vR ad
			and d' = vR da
			in
			let b' = vR a'
			and c' = vR d'
			in
			vic a  ab a' ad (n - 1);
			vic ba  b bc b' (n - 1);
			vic c' cb  c cd (n - 1);
			vic da d' dc  d (n - 1);
			vic a' b' c' d' (n - 1)
	in
	vic (50, 50) (850, 50) (850, 850) (50, 850) n
;;


let vicsek_cross n =
	let rec vic a b c d = function
		| 0 ->
			draw_line a b;
			draw_line b c;
			draw_line c d;
			draw_line d a
		| n ->
			let vR = third_vect a b
			and vU = third_vect b c
			in
			let ab = vR a
			and bc = vU b
			and dc = vR d
			and ad = vU a
			in
			let ba = vR ab
			and cb = vU bc
			and cd = vR dc
			and da = vU ad
			in
			let a' = vR ad
			and d' = vR da
			in
			let b' = vR a'
			and c' = vR d'
			in
			vic ab ba b' a' (n - 1);
			vic b' bc cb c' (n - 1);
			vic d' c' cd dc (n - 1);
			vic ad a' d' da (n - 1);
			vic a' b' c' d' (n - 1)
	in
	vic (50, 50) (850, 50) (850, 850) (50, 850) n
;;


let mandelbrot n c (minX, maxX) (minY, maxY) step =
	(* Who needs more than 50 shades of grey ? *)
	let shadeD = 255 / n
	in
	let rec mand pX pY z i shd =
		if Complex.norm z <= 2. then
			begin
				Graphics.set_color (Graphics.rgb shd shd shd);
				Graphics.plot pX pY;
				if i < n then
					mand pX pY (Complex.add (Complex.mul z z) c) (i + 1) (shd - shadeD)
				else
					()
			end
		else
			()
	in
	(* Better iter func is left as an exercice for the reader. *)
	let rec iterX (pX, x) (pY, y) =
		if x < maxX then
			begin
				iterY (pX, x) (pY, y);
				iterX (pX + 1, x +. step) (pY, y)
			end
		else
			()
	and iterY (pX, x) (pY, y) =
		if y < maxY then
			begin
				mand pX pY {Complex.re = x; Complex.im = y} 0 255;
				iterY (pX, x) (pY + 1, y +. step)
			end
		else
			()
	in
	iterX (200, minX) (200, minY)
;;


(* mountain_list 10;; *)
(* mountain 10;; *)
(* dragon 15;; *)
(* sierpinski 15;; *)
(* sierpinski_rev 15;; *)
(* vicsek 5;; *)
(* vicsek_cross 5;; *)
(* mandelbrot 20 {re = -3./.4.; im = 0.} *)
mandelbrot 20 {re = -0.835; im = -0.2321}
	(-2., 2.)
	(-1., 1.)
	2e-3
;;

(* Press start *)
read_line ();;
