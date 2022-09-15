let a =  [16;46;48;5];;

let b = [|16;46;48;5|];;

b.(2);;

b.(2) <- 155;;

Array.make_matrix 5 4 12;;

for i=10 downto 7 do print_int i; print_string "\n" done;;

(* TIPI *)
(12,12.22,"TO JE STAVEK.");;

let area51 = (12.588,85.9, "SECRET BASE");;

type koordinata = float*float*string;;

let area51:koordinata = (12.588,85.9, "SECRET BASE");;

type telefon = {
	proizvajalec:string; 
	pomnilnik:int; 
	ram:int; 
	cpu:string;
	os:string;
	mutable verzijaOS:float
	};;

let mojTelefon = {
	proizvajalec="Xiaomi"; 
  pomnilnik=256; 
  ram=8; 
  cpu="Snapdragon 870";
  os="Android";
  verzijaOS=10.};;

let mojiPhone = {
	proizvajalec="Apple"; 
  pomnilnik=256; 
  ram=8; 
  cpu="Snapdragon 870";
  os="iOS";
  verzijaOS=13.01};;

mojTelefon.os;;
mojTelefon.verzijaOS;;

mojTelefon.verzijaOS <- 11.05;;

let posodobiTelefon t = 
	t.verzijaOS <- t.verzijaOS +. 1. ;
	print_string ("Cestitke, sistem "^t.os^" ste posodobili na verzijo ");
	print_float t.verzijaOS; 
	print_string ".\n";;

posodobiTelefon mojTelefon;;
posodobiTelefon mojiPhone;;

type student = {
	letnik:int;
	spol:char;
	vpisnaST:int;
	mutable ime:string;
	mutable priimek:string;
	mutable telefon:int;
	mutable status:string
	}
	
let student1={
	ime			="Luka";
	priimek	="Matic";
	spol		='f';
	vpisnaST=123;
	telefon =2821;
	status	="porocen";
	letnik	=2000
};;

let student2={
	ime			="Eva";
	priimek	="Dostojevska";
	spol		='f';
	vpisnaST=879156;
	telefon =49871240;
	status	="complicated";
	letnik	=2001
};;
	
let poroka o1 o2 = 
	o1.status <- "porocen"; 
	o2.status <- "porocen"; 
	o1.priimek <- o1.priimek^" "^o2.priimek;
	o2.priimek <- o1.priimek;
	print_string ("Juhuhu: "^o1.ime^" in "^o2.ime^" sta zdaj porocena. Gremo pit!\n");;
	
poroka student1 student2;;

student1
student2

type barva = Rdeca | Modra | Crna | Zelena;;
type material = Usnje | Bombaz | Svila | Elastan | Kasmir | Lateks;;
type velikost = S | M | L | XL | XS;;

type obleka = barva*velikost*material;;

let hlace:obleka = Modra, L, Bombaz

type barva = 
	| RGB of int*int*int 
	| CMYK of int*int*int*int 
	| BW of int;;


BW 12;;
RGB (12,2,0);;
CMYK (0,0,0,255);;

[BW 12;RGB (12,2,0);CMYK (0,0,0,255)]

Modra;;
15.7;;

type placilo = 
	| Gotovina of int
	| Kartica of string*string*int;;

let obleka = Gotovina 15;;
let racunalnik = Kartica ("Debetna", "Eur", 900);;

let znesek n = match n with 
| Gotovina x -> x
| Kartica (x,y,z) -> z;;

znesek obleka;;
znesek racunalnik;;

let seznam = [Gotovina 15; Kartica ("Debetna", "Eur", 900); Gotovina 2; Kartica ("MasterCard", "Eur", 9200)]

let rec skupen_znesek sp = match sp with
| []   -> 0
| g::r -> znesek g + skupen_znesek r;;

skupen_znesek seznam;;

