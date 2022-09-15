 (* imperativno programiranje *)

let polje = [|1;4;42;14;2|];;

polje.(3);;

polje.(3)<-15;;

polje.(3);;

Array.make_matrix 3 2 0;;

(*  zanke *)
for i=0 to 4 do print_int i; print_string "\n" done;;

for i=10 to 4 do print_int i; print_string "\n" done;;

for i=10 downto 4 do print_int i; print_string "\n" done;;


(* TIPI *)

type trikotnik = {a:int;b:int;c:int};;

let t1= {a=4;c=5;b=3};;

t1.a 
t1.c

let vrni_hipotenuzo t = t.c;;

type trikotnik_m = {a:int;b:int;mutable h:int};;

let vrni_hipotenuzo t = t.h;;

let t2= {a=4;h=5;b=3};;
t2.h<- 15;;

t2;;

type oseba = {
	mutable ime:string;
	mutable priimek:string;
	mutable spol:char;
	mutable status:string;
	letnik:int
	}

let o1={spol='m'; ime="Denis"; priimek="Tramp"; letnik=1923; status="vdovec"};;
let o2={spol='f'; ime="Denisa"; priimek="Tramp"; letnik= 1930; status="mrtva"};;

o1;;

o1.status <- "mrtev";;

let poroka os1 os2 =  os1.status  <- "porocen"; 
											os2.status  <- "porocena"; 
											os2.priimek <- os1.priimek;;

poroka o1 o2;;

o1;;
o2;;

let k1:koordinata = (12.58,133.8,"FAMNIT");;

type koordinata = float*float*string;;


(* tipi s konstruktorji - OBLACILA *)
type barva = Modra | Rdeca | Zelena | Crna | Siva;;
type material = Usnje | Bombaz | Poliester | Svila | Lateks;;
type velikost = M | L | S | XL | XS;;

type obleka = barva*material*velikost;;

let majica:obleka = (Siva,Bombaz,M);;

type barva = RGB of int*int*int | CMYK of int*int*int*int | BW of int;;

RGB (255,255,0);;

[RGB (255,255,0);BW 15; CMYK (0,0,15,255);BW 0];;

type placilo = Gotovina of int | Kartica of string*string*int;;

Gotovina (15);;

let nakupi = [Gotovina (15); Kartica ("Master","Card",7452485182)];;

let p1 = Gotovina(500);;
let p2 = Kartica ("Master","Card",7452485182);;

let vrni_znesek p = match p with
| Gotovina x -> x
| Kartica (x,y,z) -> z;;

vrni_znesek p2;;
vrni_znesek p1;;

let rec skupni_znesek sez_n = match sez_n with
| []   -> 0
| g::r -> vrni_znesek g + skupni_znesek r;;

skupni_znesek nakupi;;

