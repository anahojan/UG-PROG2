(*n-terice*)

(2,(3,"test",5.24));;

fst (34,56);;
snd (34,56);;

(12,5.4,2)

(* seznami *)
[12;2;32]
[[12;2;32];[2;3];[]];;


List.hd [12;2;32;34;56]
List.tl [12;2;32;34;56]


0 :: [12;2;32;34;56];;

1::5::8::98::4::[];;

(* @ lepljenje*)
[12;2;32]@[12;2;32];;

(* deklaracije *)
let a = 12;;
let a = 1 and b = 2;;
let c = 1 and d = c+c;;

let c = 
	let a = 4 and b=3 in
		a*a+b*b;;

(* funkcije *)
let f = (function x -> 2*x) ;;
f 17;;

(function x -> (function y -> x*y)) 15 20;;

let g = (function x -> (function y -> x*y));;

let g = fun x y -> x*y;;

let g1 x y = x*y;;

let g2 (x,y) = x*y;;

g1 7 9;;
g2 (7,9);;

(* rekurzija *)

let rec fakulteta n = 
	if n=0 then 1
	else n*fakulteta (n-1);;

fakulteta 5;;

let rec fib n = 
	if n < 3 then 1
	else fib (n-1) + fib (n-2);;

fib 8;;

