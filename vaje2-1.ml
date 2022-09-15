(* int char string bool *)

(* n-terice, seznami *)

(12,23);;

('s','t',"test",12.);;

('s',5);;

fst ('s',5);;

snd ('s',5);;

(* fst, snd delujeta samo nad dvojkami! *)
fst ('s',5,"dfs");;

(*	SEZNAMI		*)

[1; 12; 23; 43];;

1::[12;23;43];;

1::12::[23;43];;

1::12::23::[43];;

1::12::23::43::[];;

[1; 12; 23; 43]@[1; 12; 23; 43];;

List.hd [1; 12; 23; 43];;
List.tl [1; 12; 23; 43];;

(*	DEKLARACIJE		*)

let a = 21;;
let b = 234;;

(+) a b;;

let a = 12 and b=23;;

let c = 1 and d=1;;

(* lokalne spremenljivke "in" *)

let kvadrat_hipotenuze = 
	let a=3 and b=4 in 
	  a*a+b*b


let a = 4;;

(*	FUNKCIJE  *)

function x -> x+1;;
function x -> x*x+x;;

(function x -> (function y -> x+y)) 12 23 ;;
function x -> (function y -> x+y);;


fun x -> 12-x*x;;
fun x y -> x+y;;

let vsota = function x -> (function y -> x+y);;

vsota 15 896;;

let produkt = fun x y -> x*y;;
produkt 23 5

let produkt1 = fun (x,y) -> x*y;;
produkt1 (12,48);;

let rec sestejDo = function x -> 
	if x=0 then 0 
	else x+sestejDo (x-1);;

let rec sestejDo = fun x -> 
	if x=0 then 0 
	else x+sestejDo (x-1);;

let rec sestejDo x =  
	if x=0 then 0 
	else x+sestejDo (x-1);;

sestejDo 12;;

(* fibonacci *)
let rec fib n = 
	if n<3 
		then 1 
		else fib (n-1) + fib (n-2);;
	
(* fakulteta *)
let rec fakulteta n = 
	if n=0 then 1 
	else n*fakulteta (n-1);;
	
fakulteta 5;;
	
(* funkcija ki izgradi seznam stevil do n*)

let rec seznam n = 
	if n=1 then [1] 
	else n::seznam (n-1);;

seznam 12;;

let rec sestej sez = 
	if sez=[] then 0
	else List.hd sez + sestej (List.tl sez);;

let a = [12;5;98;36;7;4];;
sestej a;;