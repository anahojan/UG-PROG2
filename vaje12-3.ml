(*error handling*)

let ( // ) a b = if b <> 0 then a/b else failwith "BUM, vse je eksplodiralo.";; 

3//15;;
12//0;;

exception Failure of string;;
raise (Failure "Tataritataratataru, igra je sla po zlu.");;

exception Moja_napaka;;
raise Moja_napaka;;

let ( // ) a b = 
	if b <> 0 
	then a/b 
	else raise Moja_napaka;; 

let ( // ) a b =
	try
		if a/b =0 then raise (Failure "BU!") else a/b
	with
	| Division_by_zero -> print_string "ojej, delitelj je nic!"; 0
	| Failure s -> print_string ("Sicer se je zgodila napaka:"^s^",\n ampak program se bo vseeno nadaljeval z vrednostjo 0.\n");0
	| _ -> 1;;

exception Advanced_napaka of (int*string);;

16//3;;
13*22- 3//0 + 45;;

(*sintaksa modulov in funktorji*)

module Ocena =
	struct
		type ocena = {mutable vrednost: int; mutable predmet:string}
		let spremeniVrednost o s = o.vrednost <- s
		let spremeniPredmet o p = o.predmet <- p
		let poglejOceno o = (o.predmet, o.vrednost)
		let ustvari = {vrednost = 0; predmet = ""}
	end;;

module type PROFESOR = 
  sig
    type ocena = { mutable vrednost : int; mutable predmet : string; }
    val spremeniVrednost : ocena -> int -> unit
    val spremeniPredmet : ocena -> string -> unit
    val poglejOceno : ocena -> string * int
    val ustvari : ocena
  end;;			

module type STUDENT = 
  sig
    type ocena = { mutable vrednost : int; mutable predmet : string; }
    val poglejOceno : ocena -> string * int
  end;;

module Prof = (Ocena:PROFESOR with type ocena = Ocena.ocena);;
module Std = (Ocena:STUDENT with type ocena = Ocena.ocena);;
let prog2=Prof.ustvari;;
Prof.spremeniPredmet prog2 "Programiranje II: Koncepti programskih jezikov."
Prof.spremeniVrednost prog2 8;;
prog2

Std.poglejOceno prog2;;
Prof.poglejOceno prog2;;

let a b = b*12;;

class mojRazred p1 p2 = 
	object
	
	end
	
module SIS (VhodniModul:PROFESOR) = 
	struct
		type ocene = {mutable baza:VhodniModul.ocena list}
		let ustvari = {baza = []}
		let pokaziOcene m = m.baza
		let vpisiOceno m o = m.baza <- o::m.baza
	end;;

module FamnitSIS = SIS(Ocena);;
let prog2=Ocena.ustvari;;
Ocena.spremeniPredmet prog2 "Programiranje II: Koncepti programskih jezikov."
Ocena.spremeniVrednost prog2 8;;
prog2;;

let bazaOcen = FamnitSIS.ustvari;;
FamnitSIS.pokaziOcene bazaOcen
FamnitSIS.vpisiOceno bazaOcen prog2;;