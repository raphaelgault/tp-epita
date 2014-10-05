#load "graphics.cma";;
open Graphics;;
open_graph "";;

open Random;;
self_init ();;

let line (x,y) (z,t) =
	moveto x y;
	lineto z t;;


let rec mountain n (x,y) (z,t) =
	match n with
		|0 -> line (x,y) (z,t); 
		|n -> let middle = (x + z)/2 and h = ((y + t)/2 + Random.int(10 * n)) in begin
		mountain (n-1) (x,y) (middle, h);
		mountain (n-1) (middle, h) (z,t);
	      end;;


let rec dragon n (x,y) (z,t) =
        match n with
                |0 -> line (x,y) (z,t);
                |n -> let  u = (x + z)/2 + (t - y)/2  and v = ((y + t)/2 - (z - x)/2) in begin
                dragon (n-1) (x,y) (u, v);
                dragon (n-1) (u, v) (z,t);
              end;;

(* determine the height of the triangle *)
let height (x,y) (z,t) =
	let xab = z - x and yab = t - y in
               let length = sqrt(float_of_int(xab*xab + yab*yab)) in
			int_of_float(sqrt(length*.length +. length/.2.));;


(* determine the middle betwin 2 points *)
let mid (x,y) (z,t) = 
	((x+z)/2,(y+t)/2);;


(* draw a triangle *)
let triangle (x,y) (z,t) = 
	let (u,v) = (mid (x,y) (z,t)) and h = height (x,y) (z,t) in
		line (x,y) (z,t);
		line (x,y) (u,v+h);
		line (u,v+h) (z,t);;
	

(* determine the vertex of a triangle *)
let vertex (x,y) (z,t) = 
	let (u,v) = (mid (x,y) (z,t)) and h = height (x,y) (z,t) in
		(u,v+h);;


(* draw the sierpinski triangle *)
let rec sierpinski n (x,y) (z,t) =
	(* let (m,n) = (mid (x,y) (z,t)) in *)
	let (u,v) = vertex(x,y) (mid (x,y) (z,t)) and (i,j) = vertex(mid (x,y) (z,t)) (z,t) in
		match n with 
			| 0 -> triangle (x,y) (z,t);
			| n -> triangle (x,y) (z,t); 
				sierpinski (n-1) (x,y) (mid (x,y) (z,t));
				sierpinski (n-1) (mid(x,y) (z,t)) (z,t);
				sierpinski (n-1) (u,v) (i,j);;



let rec cross n (x,y) w = 
	match n with
		|0 -> fill_rect x y (w/3) (w/3); 
			fill_rect x (y+(2*(w/3))) (w/3) (w/3); 
			fill_rect (x+(2*(w/3))) (y+(2*(w/3))) (w/3) (w/3);
			fill_rect (x+(2*(w/3))) y (w/3) (w/3);
			fill_rect (x+(w/3)) (y+(w/3)) (w/3) (w/3);
		|n -> cross (n-1) (x,y) (w/3);
			cross (n-1) (x,(y+(2*(w/3)))) (w/3);
			cross (n-1) ((x+(2*(w/3))),y) (w/3);
			cross (n-1) ((x+(2*(w/3))),(y+(2*(w/3)))) (w/3);
			cross (n-1) ((x+(w/3)),(y+(w/3))) (w/3);;

let rec star n (x,y) w =
	match n with
		|0 -> fill_rect (x+(w/3)) y (w/3) (w/3);
			fill_rect x (y+(w/3)) (w/3) (w/3);
			fill_rect (x+(w/3)) (y+(w/3)) (w/3) (w/3);
			fill_rect (x+(w/3)) (y+(2*(w/3))) (w/3) (w/3);
			fill_rect (x+((2*w/3))) (y+(w/3)) (w/3) (w/3);
		|n -> star (n-1) ((x+(w/3)), y) (w/3);
			star (n-1) (x,(y+(w/3))) (w/3);
			star (n-1) ((x+(w/3)),(y+(w/3))) (w/3);
			star (n-1) ((x+(w/3)),(y+(2*(w/3)))) (w/3);
			star (n-1) ((x+((2*w/3))),(y+(w/3))) (w/3);





	
