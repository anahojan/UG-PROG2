(* funkcije visjega reda *)

let a = [2;6;3;4];;
let kvadriraj = fun x -> x*x;;
List.map kvadriraj a;;

let rec map f sez = match sez with
| [] 		-> []
| hd::tl-> f hd :: map f tl;; 

map kvadriraj a;;

(* fold right *)
(* *)
(* sez = [1,2,3]*)
(* a	 = 0 			*)
(* f   = (+)    *)
(* 							*)
(* f(1,f(2,f(3,0)))  *)

List.fold_right (+) a 0;;

let rec fold_right f sez a = match sez with
| [] 		 -> a
| hd::tl -> f hd (fold_right f tl a);;

fold_right ( * ) a 100;;
	
(* filter *)
(* *)
(* for_all*)

let pozitivno x = x>0;;

pozitivno 15;;

List.for_all pozitivno (-1::a);;

let rec for_all f sez = match sez with
| []   -> true
| g::r -> if f g then for_all f r
	 				else false;;
	
for_all pozitivno (-1::a);;
	
let rec for_all f sez = match sez with
| []   					-> true
| g::r when f g -> for_all f r
| _ 						-> false;;

let urediPar (a,b) f =
	if f (a,b) then	(a,b) else (b,a)
	
let vecji (a,b) = a>b;;
	
urediPar (23,55) vecji

(* "PaRkirisCE" *)
(* 							*)
(* "pArKIRISce" *)

let str = "testna beseda";;
String.get str 4

let rec explode str i = 
	if i=String.length str then []
	else String.get str i::explode str (i+1);;

let seznamZnakov = explode str 0;;

let rec map f sez = match sez with
| [] -> []
| h::t -> f h :: map f t;;

let smallBIGcase a = if a>90 then a-32 else a+32;;

let switchCase str = 
	map char_of_int
	(map smallBIGcase
	(map int_of_char (explode str 0)));;
	
switchCase "BaNaNa"
(* *)
(* *)

	
	
	
	