
(*error handling*)

let deljenje a b = 
	if b <> 0 
	then a/b 
	else failwith "tole pa ne gre!";;

deljenje 16 0;;

failwith "tole pa ne gre!"

exception Moja_napaka of int*string;;

raise (Moja_napaka (15,""));;

let ( // ) a b = 
	try
		match a/b with 
		| 0 -> raise (Moja_napaka (b,""))
		| _ -> a/b
	with
	| Moja_napaka (0,_) -> print_string "nic posebnega, nadaljuj";0
	| Moja_napaka (42,_)-> print_string "ustavi se in popij kavo";42
	| Moja_napaka (st,_) -> st
	| Division_by_zero -> print_string "Hej, poglej imenovalec..\n"; 0
	| Failure s -> print_string ("Zgodila se je Falilure napaka s stringom: "^s);0
	| _ -> 0;;

16 // 0;;

(*funktorji*)

let a b = (b*15);;,

module Ocena = 
	struct
		type ocena 
		let spremeniVrednost m vr = m.vrednost <- vr
		let spremeniPredmet m pr = m.predmet <- pr
		let poglejOceno m = (m.vrednost, m.predmet)
		let ustvari = {vrednost=0; predmet=""}
	end;;

module type PROFESOR =
  sig
    type ocena 
    val spremeniVrednost : ocena -> int -> unit
    val spremeniPredmet : ocena -> string -> unit
    val poglejOceno : ocena -> int * string
    val ustvari : ocena
  end;;
	
module type STUDENT = 
  sig
    type ocena = { mutable predmet : string; mutable vrednost : int; }
    val poglejOceno : ocena -> int * string
  end;;
	
	
module Prof = (Ocena:PROFESOR with type ocena = Ocena.ocena);;
module Std  = (Ocena:STUDENT  with type ocena = Ocena.ocena);;

let prog2=Prof.ustvari;;
Prof.spremeniVrednost prog2 9;;
Prof.spremeniPredmet prog2 "Programiranje 2: Koncepti programskih jezikov";;
prog2
Std.poglejOceno prog2
	
module SIS (Ocena:PROFESOR) =
	struct
		type ocene = {mutable sez: Ocena.ocena list}
		let vpisiOceno baza (ocena:Ocena.ocena) = baza.sez <- ocena::baza.sez 
		let pokaziOcene m = m.sez
		let ustvari = {sez = []}
	end
	
module FamnitSIS = SIS (Ocena);;
let prog2=Ocena.ustvari;;
Ocena.spremeniVrednost prog2 9;;
Ocena.spremeniPredmet prog2 "Programiranje 2: Koncepti programskih jezikov";;
prog2

let infSistem = FamnitSIS.ustvari;;
infSistem
FamnitSIS.vpisiOceno infSistem prog2;;


	