(*  UJEMANJE VZORCEV 	 *)
(*										 *)
(* (pattren matching)  *)

let teden = fun x -> 
	if x=1 then "ponedeljek" else
	if x=2 then "torek" else
	if x=3 then "sreda" else
	if x=4 then "cetrtek" else
	if x=5 then "petek" else
	if x=6 then "sobota" else
	if x=7 then "nedelja" else "NAPAKA!";;

teden 5;;

let teden = function
	| 1 -> "ponedeljek" 
	| 2 -> "torek" 
	| 3 -> "sreda" 
	| 4 -> "cetrtek" 
	| 5 -> "petek" 
	| 6 -> "sobota" 
	| 7 -> "nedelja"
	| _ -> "NAPAKA, vnesi stevilo med 1 in 7.";;
	
let teden x = match x with
	| 1 -> "ponedeljek" 
	| 2 -> "torek" 
	| 3 -> "sreda" 
	| 4 -> "cetrtek" 
	| 5 -> "petek" 
	| 6 -> "sobota" 
	| 7 -> "nedelja"
	| _ -> "NAPAKA, vnesi stevilo med 1 in 7.";;


let koncnaOcena dn1 dn2 dn3 izpit = match dn1+dn2+dn3+izpit with
| 100 -> "Odlicno! Pripada vam vecna cast in slava."
| _ as kt when kt>90 -> "ocena 10"
| _ as kt when kt>80 -> "ocena 9"
| _ as kt when kt>70 -> "ocena 8 "
| _ as kt when kt>60 -> "ocena 7 "
| _ as kt when kt>50 -> "ocena 6 "
| _ as kt -> "Na zalost vam tokrat ni uspelo, dosegli ste namrec le "
							^ (string_of_int kt) ^ "<50 tock.";;

koncnaOcena 10 10 10 12;;

let jeSamoglasnik = function 
	| 'a' | 'e' | 'i' | 'o' | 'u' -> true
	| _ -> false ;;


(* let rec dolzina = function *)
	 let rec dolzina seznam = match seznam with
	| [] 	 -> 0
	| g::r -> 1 + (dolzina r);;

dolzina [2;32;4;2;6];;

(* obrni seznam *)
let rec obrni = function
	| [] 		 -> []
	| hd::tl -> (obrni tl)@[hd];;

obrni [2;32;4;2;6];;

let rec vsota sez = match sez with
| []   -> 0
| g::r -> vsota r + g;;

vsota [2;32;4;2;6];;

(*  funkcije visjega reda  *)


List.map (fun x->x+10) [2;32;4;2;6];;

List.map (fun x->x/2) [2;32;4;2;6];;

let ocene = [10;8;5;9;6;5;3;11];;
let f x = match x with
| _ when x>5 && x<11 -> "opravil"
| _ when x>0 && x<6 -> "ni opravil"
| _ -> "napaka v oceni";;

List.map f ocene;;

let rec map f seznam = match seznam with
| [] -> []
| h::r -> f h :: map f r

List.filter (fun x -> x>5)  [10;8;5;9;6;5;3;11];;

let rec filter f seznam = match seznam with
| [] 						-> []
| g::r when f g -> g :: filter f r
| _::r 					-> filter f r;;
	
filter (fun x -> x>5)  [10;8;5;9;6;5;3;11];;
