(* error handling *)

type integer = Val of int | Error ;;
let ( // ) a b = match b with
| 0 -> Error
| _ -> Val (a/b);;

16//0;;

(* defining exceptions *)

12/0;;

exception Moja_napaka;;

exception Zanimiva_napaka of string;;

let ( // ) a b = if b = 0 
	then raise (Zanimiva_napaka "deljenje z niclo!")
	else a/b;;

12//0;;

let ( // ) a b = if b = 0 
	then failwith "tole ne bo delovalo.. "
	else a/b;;

let ( // ) a b = 
	try
		a/b
	with
	| Failure s -> print_string ("Pojavila se je napaka s sporocilom: "^s);0 
	| Division_by_zero -> print_string "Delil si z nic :(\n";0;;


(* functors *)
module Ocena = 
	struct
		type ocena = { mutable vrednost:int; mutable predmet:string}
		let spremeniVrednost m vr = m.vrednost <- int_of_string vr
		let spremeniPredmet m pr = m.predmet <- pr
		let poglejOceno m = (m.predmet,m.vrednost)
		let ustvari = {vrednost=0; predmet = ""}
	end;;


	
module Ocena2 = 
	struct
		type ocena = { mutable vrednost:string; mutable predmet:string}
		let spremeniVrednost m vr = m.vrednost <- vr
		let spremeniPredmet m pr = m.predmet <- pr
		let poglejOceno m = (m.predmet,m.vrednost)
		let ustvari = {vrednost=""; predmet = ""}
	end;;

module type PROFESOR =
  sig
    type ocena
    val spremeniVrednost : ocena -> string -> unit
    val spremeniPredmet : ocena -> string -> unit
    val poglejOceno : ocena -> string * string
    val ustvari : ocena
  end

module type STUDENT = 
  sig
    type ocena
    val poglejOceno : ocena -> string * string
  end;;
	
	

let a = function b -> 21*b;;

module SIS = functor (Ocena:PROFESOR) ->
	struct
		type ocene = {mutable ocene: (string*string) list}
		let vpisiOceno m ocena = m.ocene <- (Ocena.poglejOceno ocena) :: m.ocene
		let pokaziOcene m = m.ocene
		let ustvari = { ocene = []}
	end;;

let prog2 = Profesor.ustvari;;
Profesor.spremeniVrednost prog2 "9";;
Profesor.spremeniPredmet prog2 "Programiranje II: Koncepti programskih jezikov";;
prog2;;

module Profesor = (Ocena2:PROFESOR with type ocena = Ocena2.ocena);;
module Student = (Ocena2:STUDENT with type ocena = Ocena2.ocena);;

Profesor.poglejOceno prog2;;
Profesor.spremeniVrednost prog2 "10";;
Student.poglejOceno prog2;;

module FamnitSIS = SIS(Ocena2);;	

let bazaOcen = FamnitSIS.ustvari;;
FamnitSIS.vpisiOceno bazaOcen prog2;;
FamnitSIS.pokaziOcene bazaOcen;;