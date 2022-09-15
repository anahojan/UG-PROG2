let polje = [|12;5;8;68;4580|];;

polje.(3);;

polje.(3) <- 69;;

Array.make_matrix 3 5 '%';;

for i=10 downto 6 do print_int i; print_string "\n" done;;

(* TIPI *)

type predavalnica = {
	st_stolov:int; 
	sifra:string; 
	mutable projektor:bool;
	vrsta:string
};;

let p1={
	st_stolov=24; 
	projektor=false; 
	vrsta="racunalnica"; 
	sifra="RLab1"};;

p1.st_stolov;;

let kapaciteta p = p.st_stolov-1;;

kapaciteta p1

p1.projektor <- true;;

let vgradi_projektor p = 
	if p.projektor then 
		print_string ("V predavalnici "^p.sifra^" je projektor ze vgrajen.\n")
	else
	(p.projektor <- true; 
	print_string ("V predavalnico "^p.sifra^" smo uspesno vgradili projektor.\n"));;


let p2={
	st_stolov=14; 
	projektor=false; 
	vrsta="mala predavalnica"; 
	sifra="MP1"};;

vgradi_projektor p2;;
p2

let lokacija1:koordinata = (12.58,89.8,"FAMNIT");;

type koordinata = float * float * string ;;

(*TIPI IN KONSTRUKTORJI*)

type barva_kartuse = Cyan | Magenta | Yellow | Black;;
type kapaciteta = S | M | L;;
Cyan;;
M;;

type barva = 
	| CMYK of int*int*int*int
	| RGB  of int*int*int
	| BW 	 of int;;

CMYK (12,87,255,255);;
BW 15;;
RGB (255,0,0);;

[CMYK (12,87,255,255);BW 15;RGB (255,0,0)]

let je_sivina b = match b with 
| BW x -> true
| RGB (x,y,z) when x=y && y=z -> true
| CMYK (x,y,z,_) when x+y+z=0 -> true
| _ -> false

