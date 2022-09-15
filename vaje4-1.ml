(* f. visjega reda *)
(* *)
(* *)
(* map *)

let for_all pozitiven [1; 2; 3; 4; -6];; = [1;2;3;4;6];;

List.map (fun x->x+1) a;;

let rec map f sez = match sez with
| [] 		 -> []
| hd::tl -> f hd :: map f tl;;

map (fun x->x+1) a;;

(* fold right 		*)
(* f							*)
(* a1,a2,a3 			*)
(* b 							*)
(* f(a1,f(a2, f(a3,b))) *)

List.fold_right (+) a 100;;

(+) 12 13;;

let rec fold_right f lst b = match lst with
| [] 			 -> b
| hd :: tl -> f hd (fold_right f tl b);;

fold_right (+) a 100;;

(* fold left 		*)
(* f							*)
(* a1,a2,a3 			*)
(* a 							*)
(* f(f(f(a,a1),a2),a3) *)

let rec fold_left f a lst = match lst with
| [] -> a
| hd::tl -> f (fold_left f a tl) hd;;

fold_left (+) 100 a;;

(* List.filter *)

(* List.for_all *)
(* *)

let pozitiven = function x -> x>0;;

let pozitiven x = x>0;;

pozitiven (-12)

List.for_all pozitiven a;;
List.for_all pozitiven [1; 2; 3; 4; -6];;

a;;

let rec for_all f sez = match sez with
| [] 	 					-> true
| g::r when f g -> for_all f r 
| _ 						-> false;;

for_all pozitiven a;;
for_all pozitiven [1; 2; 3; 4; -6];;

3<6;;
'c'>'f';;

let vecji (a,b) = a>b;;

(* Napisi funkcijo, ki iz seznama odstrani vsa liha stevila *)

let odstrani_liha sez = List.filter (fun x -> x mod 2 = 0) sez;;

let odstrani_liha = List.filter (fun x -> x mod 2 = 0);;

let rec odstrani_liha sez = match sez with
| [] 		 									 -> []
| hd::tl when hd mod 2 = 0 -> hd::odstrani_liha tl
| _ ::tl									 ->     odstrani_liha tl;;



odstrani_liha [1; 2; 3; 4; -6];;


(*Naredi funkcijo, ki dobi seznam:int list, vrne pa seznam tipa bool list, kjer
je true v primeru, da je stevilo sodo in false v primeru, da je stevilo liho*)

let rec sodaliha sez = match sez with
| [] 		 -> []
| hd::tl -> (hd mod 2 = 0)::sodaliha tl;;
	
sodaliha [1; 2; 3; 4; -6];;
