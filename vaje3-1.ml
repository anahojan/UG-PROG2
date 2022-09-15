(* ujemanje vzorce *)
(* *)
(* pattern matching *)

let teden x = 
	if x=1 then 
		"ponedeljek" 
	else
		if x=2 then 
			"torek" 
		else
			if x=3 then 
				"sreda" 
			else
				if x=4 then 
					"cetrtek" 
				else
					if x=5 then 
						"petek" 
					else	
						if x=6 then 
							"sobota" 
						else	
							if x=7 then 
								"nedelja" 
							else 
								"napaka!";;

let teden x = match x with
| 1 -> "ponedeljek"
| 2 -> "torek"
| 3 -> "sreda"
| 4 -> "cetrtek"
| 5 -> "petek"
| 6 -> "sobota"
| 7 -> "nedelja"
| _ -> "napaka";;


let implikacija p1 p2 = match (p1,p2) with
| (true,true) -> true
| (false,true) -> true
| (true,false) -> false
| (false,false) -> true;;

let implikacija x = match x with
| (true,true) -> true
| (false,true) -> true
| (true,false) -> false
| (false,false) -> true;;

let implikacija x = match x with
| (true,false) -> false
| _ -> true;;


let implikacija x = match x with
| (true,true) | (false,true) | (false,false) -> true
| (true,false) -> false;;

let jeSamoglasnik crka = match crka with 
|'a'|'e'|'i'|'o'|'u' -> true
|'A'|'E'|'I'|'O'|'U' -> true
| _ -> false;;


jeSamoglasnik 'e';;
jeSamoglasnik 'p';;
jeSamoglasnik '&';;
jeSamoglasnik 'E';;

let enakost = fun a b -> a=b;;

let enakost a b = match a with
| _ when a=b -> true
| _ -> false

enakost 5 5;;
enakost true false;;


let vsotaAliProdukt (a,b,kaj) = match kaj with 
| "vsota" -> a+b
| "produkt" -> a*b
| _ -> 0;;

vsotaAliProdukt (23,34,"vsota");;

let vsotaAliProdukt2 (a,b,kaj) = match (a,b,kaj) with 
| (_,_,"vsota") 						-> a+b, (a,b,kaj)
| (_,_,"produkt")          	-> a*b, (a,b,kaj)
| _ as param 								->   0, param;;

let rec dolzina seznam = 
	if seznam=[] then 0 else 
	1+dolzina (List.tl seznam);;

let rec dolzina seznam = match seznam with
| [] -> 0
| g::r -> 1 + (dolzina r);;

dolzina [154;15;8;7;98];;

let drugiElement seznam = match seznam with
| [] | _::[] -> 0
| _::drugi::_ -> drugi;;

drugiElement [154;15;8;7;98];;
drugiElement [154];;

(*Napisi funkcijo, ki obrne seznam z *)
(* uporabo ujemanja vzorcev.*)

let rec obrni seznam = match seznam with
| [] 	 -> []
| g::r -> (obrni r) @ [g];;

(*funkcije visjega reda*)

List.map (fun x->x+1) [123;2;34;-2];;

let rec map f sez = match sez with 
| [] -> []
| g::r -> f g :: (map f r);;

map (fun x->x+1) [123;2;34;-2];;

List.filter (fun x -> x>10) [123;2;34;-2];;

let rec filter f sez = match sez with
| [] -> []
| g::r when f g -> g :: filter f r
| _::r -> filter f r;;


