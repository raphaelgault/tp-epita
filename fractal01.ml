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

let height (x,y) (z,t) =
	let xab = z - x and yab = t - y in
               let length = sqrt(float_of_int(xab*xab + yab*yab)) in
			int_of_float(sqrt(length*.length +. length/.2.));;


let mid (x,y) (z,t) = 
	((x+z)/2,(y+t)/2);;


let triangle (x,y) (z,t) = 
	let (u,v) = (mid (x,y) (z,t)) and h = height (x,y) (z,t) in
		line (x,y) (z,t);
		line (x,y) (u,v+h);
		line (u,v+h) (z,t);;
	


let rec sierpinski n (x,y) (z,t) =
	match n with 
		| 0 -> triangle (x,y) (z,t);
		| n -> triangle (x,y) (z,t); 
			sierpinski (n-1) (x,y) (mid (x,y) (z,t));
			sierpinski (n-1) (mid(x,y) (z,t)) (z,t);
			(*sierpinski (n-1) (mid (mid(x,y) (z,t)) (x,y),(heigth(x,y) (z,t))/2) ((mid (mid(x,y) (z,t)) (z,t),(heigth(x,y) (z,t))/2));;*)

	
