(* ujemanje vzorcev *)

let teden = function
	| 1 -> "ponedeljek"
	| 2 -> "torek"
	| 3 -> "sreda"
	| 4 -> "cetrtek"
	| 5 -> "petek"
	| _ -> "vikend";;

let teden x = match x with
	| 1 -> "ponedeljek"
	| 2 -> "torek"
	| 3 -> "sreda"
	| 4 -> "cetrtek"
	| 5 -> "petek"
	| _ -> "vikend";;

let ocenaProgII dn1 dn2 dn3 pi = match dn1+dn2+dn3+pi with
| 100 -> "Odlicno, vse tocke! Pripada vam vecna slava."
| _ as tocke -> "Kar v redu; vasa ocena: "^(string_of_int (1+tocke/10));;

ocenaProgII 10 10 10 53;;


let ocenaProgII dn1 dn2 dn3 pi = match dn1+dn2+dn3+pi with
| 100 -> "Odlicno, vse tocke! Pripada vam vecna slava."
| _ as tocke when tocke<0		-> "Nekaj je narobe z vasimi tockami."
| _ as tocke when tocke<50	-> "Na zalost vam tokrat ni uspelo."
| _ as tocke -> "Kar v redu; vasa ocena: "^(string_of_int (1+tocke/10));;

ocenaProgII 10 10 10 53;;
ocenaProgII 10 10 10 (-53);;
ocenaProgII 10 10 10 5;;
ocenaProgII 10 10 10 20;;