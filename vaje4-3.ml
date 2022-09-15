(* funkcije visjega reda *)

(* map *)

let a = [27.8;78.;15.5;0.1];;

let kakoToplo x = if x>25. then "vroce" else
	if x<19. then "mrzlo" else "OK";;

kakoToplo 235.3;;

List.map kakoToplo a;;

let rec map f sez = match sez with
| [] 		 -> []
| hd::tl -> f hd :: map f tl;;

map kakoToplo a;;

(* fold right *)
let rec fold_right f sez a = match sez with
| [] 		 -> a
| hd::tl -> f hd (fold_right f tl a);;

fold_right (+.) a 50.;;

(* List.filter *)
let rec filter f sez = match sez with
| []->[]
| h::t when f h -> h::filter f t
| _::t -> filter f t;;

(* for_all *)
List.for_all (fun x-> x>0.0) a;;
List.for_all (fun x-> x>0.0) ((-54.5)::a);;

