(* int bool float char string *)

"test";;

(*n-terice*)

(12,45);;

(12,3.14,'c');;

(*delujeta samo pri 2-tericah*)
fst (12,45);;
snd (12,45);;

(* SEZNAMI *)

[1;23;2;34;40];;

List.hd [1;23;2;34;40];;
List.tl [1;23;2;34;40];;

List.hd (List.tl (List.tl [1;23;2;34;40]));;
(* raje ne uporabljamo *)
List.nth [1;23;2;34;40] 2;;

[1;23;2;34;40];;
1::[23;2;34;40];;
1::23::[2;34;40];;

1::23::2::34::40::[];;

[1;23;2;34;40]@[1;23;2;34;40];;


(* deklaracije *)

let a = "Hello World!" ;;
let b = 12;;

let a = 34 and b = 3.14;;

let a = 12+12;;

let a = 1 and b = a+a;;

let c = 
	let a = 3 and b=4 in
	a*a+b*b;;

(* funkcije *)

(function x->2*x) 5;;

(function x -> (function y -> x+y)) 5 12;;

(fun x y -> x+y) 5 12 ;;

let vsota = fun x y -> x+y;;

vsota 12 5;;

(* rekurzivne funkcije *)
let rec sestejDo = 
	function n -> if n=0 then 0 
	else n + sestejDo (n-1);;

let rec sestejDo = fun n -> if n=0 then 0 
	else n + sestejDo (n-1);;

let rec sestejDo n = if n=0 then 0 
	else n + sestejDo (n-1);;

sestejDo 12;;

(* zgledi*)
(* *)
(* *)
(* Fibonaccijevo stevilo *)

let rec fib n = 
	if n<2 then 1
	else fib (n-1) + fib (n-2);;

fib 12;;

(* fakulteta stevila *)
let rec fakulteta n = 
	if n=0 then 1 
	else n*fakulteta (n-1);;

fakulteta 5;;

(* funkcija, ki generira seznam stevil od n ... 1*)

let rec seznamStevil n = 
	if n=1 then [1]
	else n :: seznamStevil (n-1);;

seznamStevil 14;;

(* funkcija ki sesteje seznam stevil *)
let rec sestejSeznam s = 
	if s=[] then 0
	else (List.hd s) + sestejSeznam (List.tl s);;

sestejSeznam (1::23::[2;34;40]);;

()