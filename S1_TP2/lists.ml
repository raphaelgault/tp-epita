let rec print_int_list = function
    | h::t ->
        print_int h;
        print_int_list t
    | _ ->
        print_endline ""
;;


let merge_sorted l1 l2 =
    let rec merge r = function
        | (h1::t1, h2::t2) when h1 = h2 -> merge (r@[min h1 h2]) (t1, t2)
        | (h1::t1, h2::t2) when h1 > h2 -> merge (r@[h2]) (h1::t1, t2)
        | (h1::t1, h2::t2) -> merge (r@[h1]) (t1, h2::t2)
        | (h::t, []) | ([], h::t) -> merge (r@[h]) ([], t)
        | _ -> r
    in
    merge [] (l1, l2)
;;


(*
let rec flatten = function
    | h::t ->
        begin
            match h with
                | _::_ -> flatten h @ flatten t
                | _ -> h::(flatten t)
        end
    | _ -> []
;;
*)

let rec flatten = function
    | [] -> []
    | l::r -> l @ flatten r
;;

(* merge_sorted [1; 2; 5; 6] [1; 7];; *)
(* print_int_list (flatten [[1; 2; 3]; [[[[5]]]]]);; *)
print_int_list (flatten [[1; 2; 3]; [5]]);;
