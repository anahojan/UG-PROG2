(**1. Tip fTree predstavlja ukorenjeno druˇzinsko drevo, kjer je n ime korena druˇzinskega
drevesa, g je njegov spol, s je ime soproga/e (ki je nasprotenega spola), in c pred-
stavlja (potencialno prazno) polje z njunimi otroki (ki imajo lahko rekurzivno
svoje otroke).*)


type gender=M|F;;
type fTree = {n:string; s:string;g:gender;c: fTree array};;

let tree1={n="Edrice";s="Nkosi";g=F;c=[|{n="Waaiz";s="Dendera";g=M;c=[|{n="Bithiah";s="Ife";g=M;c=[|{n="Baahir";s="Nephthys";g=M;c=[||]};{n="Nane";s="Abayomi";g=F;c=[||]};|]};{n="Eshe";s="Uthman";g=F;c=[|{n="Bahiti";s="Amr";g=F;c=[||]};{n="Habibah";s="Djoser";g=F;c=[||]};|]};|]};{n="Nour";s="Sef";g=F;c=[|{n="Khepri";s="Hager";g=F;c=[|{n="Thema";s="Apep";g=F;c=[||]};{n="Femi";s="Eboney";g=M;c=[||]};{n="Saadah";s="Chione";g=M;c=[||]};|]};{n="Bastet";s="Nephi";g=F;c=[|{n="Nkosi";s="Zuberi";g=M;c=[||]};|]};|]};|]};;

let tree2=
{n="Baako";s="Masika";g=M;c=[|{n="Heqet";s="Hemede";g=F;c=[|{n="Ammon";s="Salma";g=M;c=[|{n="Chione";s="Odion";g=F;c=[||]};{n="Amenhotep";s="Eboni";g=M;c=[||]};{n="Zosar";s="Zuberi";g=M;c=[||]};|]};{n="Rehema";s="Nkosi";g=F;c=[|{n="Shakir";s="Bennu";g=M;c=[||]};|]};{n="Rana";s="Mido";g=F;c=[|{n="Bennu";s="Maat";g=F;c=[||]};{n="Ozymandias";s="Mandisa";g=M;c=[||]};|]};|]};|]};;

let tree3=
{n="Nanu";s="Sef";g=F;c=[|{n="Montu";s="Khepri";g=M;c=[|{n="Eboni";s="Odion";g=F;c=[|{n="Montu";s="Khepri";g=M;c=[|{n="Amon";s="Onofria";g=M;c=[||]};{n="Maye";s="Makalani";g=F;c=[||]};|]};{n="Odion";s="Neith";g=M;c=[|{n="Pilis";s="Tiye";g=M;c=[||]};|]};|]};|]};{n="Yahya";s="Nour";g=M;c=[|{n="Bast";s="Neferteri";g=M;c=[|{n="Thutmose";s="Bennu";g=M;c=[|{n="Hathor";s="Khentimentiu";g=F;c=[||]};|]};{n="Bennu";s="Zuberi";g=F;c=[|{n="Sa'd";s="Habibah";g=M;c=[||]};|]};|]};{n="Zuberi";s="Omar";g=F;c=[|{n="Midge";s="Bes";g=F;c=[|{n="Moswen";s="Moisis";g=F;c=[||]};|]};{n="Abayomi";s="Dalilah";g=M;c=[|{n="Zuberi";s="Ozymandias";g=F;c=[||]};|]};{n="Sef";s="Tanith";g=M;c=[|{n="Safiya";s="Moise";g=F;c=[||]};{n="Onofria";s="Moswen";g=F;c=[||]};|]};|]};|]};{n="Neferteri";s="Akl";g=F;c=[|{n="Massika";s="Mido";g=F;c=[|{n="Bast";s="Edrice";g=M;c=[|{n="Kosey";s="Lotus";g=M;c=[||]};|]};{n="Anat";s="Hager";g=F;c=[|{n="Moeshe";s="Rana";g=M;c=[||]};{n="Mandisa";s="Moises";g=F;c=[||]};{n="Cepos";s="Keket";g=M;c=[||]};|]};|]};|]};|]};;

let tree4=
{n="Aharon";s="Nour";g=M;c=[|{n="Tauret";s="Odion";g=F;c=[|{n="Chafulumisa";s="Chione";g=M;c=[|{n="Akila";s="Menefer";g=F;c=[|{n="Mariam";s="Naguib";g=F;c=[||]};{n="Bast";s="Asenath";g=M;c=[||]};{n="Nephthys";s="Sa'd";g=F;c=[||]};|]};|]};|]};|]};;

let tree5=
{n="Menefer";s="Auset";g=M;c=[|{n="Midge";s="Aharon";g=F;c=[|{n="Khepri";s="Saadah";g=F;c=[|{n="Moishe";s="Kissa";g=M;c=[|{n="Nefret";s="Menefer";g=F;c=[|{n="Ife";s="Yahya";g=F;c=[|{n="Salma";s="Pilis";g=F;c=[||]};|]};|]};{n="Moeshe";s="Lateefah";g=M;c=[|{n="Apep";s="Fukayna";g=M;c=[|{n="Tiye";s="Apophis";g=F;c=[||]};{n="Auset";s="Abubakar";g=F;c=[||]};|]};{n="Taafeef";s="Eboni";g=M;c=[|{n="Thema";s="Menefer";g=F;c=[||]};{n="Hafsah";s="Bahadur";g=F;c=[||]};{n="Zosar";s="Thema";g=M;c=[||]};|]};|]};|]};{n="Maye";s="Hager";g=F;c=[|{n="Nanu";s="Nane";g=F;c=[|{n="Makalani";s="Nefret";g=M;c=[|{n="Shabaka";s="Edrice";g=M;c=[||]};{n="Odion";s="Tiye";g=M;c=[||]};|]};|]};{n="Bassel";s="Eboni";g=M;c=[|{n="Maibe";s="Hemede";g=F;c=[|{n="Ebonee";s="Apophis";g=F;c=[||]};|]};|]};|]};{n="Isis";s="Sef";g=F;c=[|{n="Habibah";s="Amon";g=F;c=[|{n="Nephthys";s="Apep";g=F;c=[|{n="Zahur";s="Akila";g=M;c=[||]};{n="Jabare";s="Amunet";g=M;c=[||]};{n="Abubakar";s="Jomana";g=M;c=[||]};|]};{n="Tiye";s="Lateef";g=F;c=[|{n="Femi";s="Hafsah";g=M;c=[||]};|]};|]};|]};|]};{n="Eboni";s="Hanbal";g=F;c=[|{n="Ra";s="Tanit";g=M;c=[|{n="Zosar";s="Keket";g=M;c=[|{n="Kosey";s="Anat";g=M;c=[|{n="Moswen";s="Hemeda";g=F;c=[||]};{n="Berenike";s="Aten";g=F;c=[||]};{n="Hemeda";s="Kleopatra";g=M;c=[||]};|]};{n="Shakir";s="Auset";g=M;c=[|{n="Khepri";s="Bes";g=F;c=[||]};{n="Rabia";s="Khufu";g=F;c=[||]};|]};|]};|]};|]};|]};{n="Menes";s="Nefertari";g=M;c=[|{n="Bahiti";s="Maat";g=F;c=[|{n="Moisis";s="Nailah";g=M;c=[|{n="Mandisa";s="Jabari";g=F;c=[|{n="Dakarai";s="Bastet";g=M;c=[|{n="Jabari";s="Bahiti";g=M;c=[||]};{n="Nefret";s="Imhotep";g=F;c=[||]};|]};{n="Ebonee";s="Abraxas";g=F;c=[|{n="Tiye";s="Akhenaten";g=F;c=[||]};{n="Abasi";s="Hathor";g=M;c=[||]};{n="Masud";s="Nanu";g=M;c=[||]};|]};|]};|]};{n="Femi";s="Khufu";g=F;c=[|{n="Thutmose";s="Eboni";g=M;c=[|{n="Akl";s="Zuberi";g=M;c=[|{n="Hemeda";s="Chione";g=M;c=[||]};|]};{n="Zosar";s="Kissa";g=M;c=[|{n="Tanith";s="Ammon";g=F;c=[||]};{n="Maye";s="Bahman";g=F;c=[||]};|]};{n="Asenath";s="Abdelrahman";g=F;c=[|{n="Naguib";s="Rabia";g=M;c=[||]};|]};|]};{n="Ebonee";s="Bast";g=F;c=[|{n="Bahadur";s="Midge";g=M;c=[|{n="Eshe";s="Pilis";g=F;c=[||]};{n="Bahiti";s="Jabari";g=F;c=[||]};|]};{n="Khnurn";s="Massika";g=M;c=[|{n="Aten";s="Ebonee";g=M;c=[||]};{n="Nephthys";s="Ur-Atum";g=F;c=[||]};|]};|]};|]};|]};{n="Hemede";s="Femi";g=M;c=[|{n="Nkosi";s="Anat";g=M;c=[|{n="Neferure";s="Abubakar";g=F;c=[|{n="Nefertari";s="Rah";g=F;c=[|{n="Lotus";s="Moises";g=F;c=[||]};|]};{n="Moke";s="Eboni";g=M;c=[|{n="Baahir";s="Neferure";g=M;c=[||]};{n="Baniti";s="Eboney";g=M;c=[||]};{n="Aharon";s="Tiye";g=M;c=[||]};|]};|]};|]};|]};|]};|]};;

let tree6=
{n="Akhenaten";s="Salma";g=M;c=[|{n="Ammon";s="Kissa";g=M;c=[|{n="Midge";s="Maat";g=F;c=[|{n="Ra";s="Nephthys";g=M;c=[|{n="Eshaq";s="Nenet";g=M;c=[|{n="Isis";s="Kamuzu";g=F;c=[|{n="Amon";s="Tanit";g=M;c=[||]};{n="Moises";s="Lateefah";g=M;c=[||]};{n="Bennu";s="Baahir";g=F;c=[||]};|]};|]};{n="Auset";s="Masudi";g=F;c=[|{n="Mido";s="Mandisa";g=M;c=[|{n="Fukayna";s="Khnurn";g=F;c=[||]};{n="Tanith";s="Omar";g=F;c=[||]};|]};{n="Abasi";s="Bahiti";g=M;c=[|{n="Donkor";s="Nailah";g=M;c=[||]};|]};|]};|]};|]};{n="Feme";s="Akl";g=F;c=[|{n="Ebony";s="Khentimentiu";g=F;c=[|{n="Dalilah";s="Abanoub";g=F;c=[|{n="Dakarai";s="Asenath";g=M;c=[|{n="Hemeda";s="Lapis";g=M;c=[||]};|]};|]};{n="Fukayna";s="Seb";g=F;c=[|{n="Bahman";s="Moswen";g=M;c=[|{n="Moises";s="Bahiti";g=M;c=[||]};|]};{n="Lotus";s="Amon";g=F;c=[|{n="Farida";s="Chafulumisa";g=F;c=[||]};{n="Feme";s="Cepos";g=F;c=[||]};{n="Asenath";s="Masuda";g=F;c=[||]};|]};|]};|]};|]};|]};{n="Eshaq";s="Neferteri";g=M;c=[|{n="Bastet";s="Khnurn";g=F;c=[|{n="Pilis";s="Maye";g=M;c=[|{n="Shabaka";s="Anat";g=M;c=[|{n="Lateefah";s="Makalani";g=F;c=[|{n="Moishe";s="Tiye";g=M;c=[||]};{n="Anippe";s="Aten";g=F;c=[||]};{n="Eshe";s="Aten";g=F;c=[||]};|]};{n="Bassel";s="Eboney";g=M;c=[|{n="Chafulumisa";s="Asenath";g=M;c=[||]};|]};{n="Maye";s="Hemeda";g=F;c=[|{n="Moeshe";s="Massika";g=M;c=[||]};|]};|]};|]};|]};|]};|]};;



(*apiˇsite funkcijo countfm : fTree -> int * int = <fun> ki za vhoden
fTree izraˇcuna ˇstevilo potomcev ˇzenskega in ˇstevilo potomcev moˇskega spola
(vkljuˇcno s korenom, soprogi ne ˇstejejo, ker niso v krvnem sorodstvu)*)

let countfm vd =

  let rec check_gender vd (f,m) = match vd.g with
  | M -> Array.fold_right check_gender vd.c (f, m+1)
  | F -> Array.fold_right check_gender vd.c (f+1,m)
in 
  check_gender vd (0,0);;

let childless (vd:fTree) =
  let rec otroki (polje:fTree array) (vd:fTree) = match vd.c with
  | _-> Array.fold_left (fun x y -> if (y.c == [||]) then Array.append x [|y|] else otroki x y) polje vd.c
in 
Array.to_list(otroki [||] vd);;




  (*grafovski model*)
type node = int;;
type graph = (node*node list) list;;





module Graph : 
  sig
    type node = int
    type graph = (node*node list) list
    val order: graph -> int
    val degree: graph -> node -> int
    (*val max_degree : graph -> int
    val min_degree : graph -> int
    val matrix_of_graph : graph -> int array array
   val add_edge : graph -> node * node -> graph
    val delete_last : graph -> graph
    val add : graph -> node list -> graph*)
  end
=
  struct
    type node = int
    type graph = (node*node list) list
    let order (g:graph)  = (List.length g)
    let rec degree (g:graph) (stopinje:node) = match g with
      | (stopinje,x)::r -> List.length x
      | (_,_)::r   -> degree r stopinje
      | [] -> -1
  end
;;




let claw:Graph.graph = [1,[2;3;4];2,[1];3,[1];4,[1]] ;;


(*3. V univerzitetnem informacijskem sistemu so ljudje klasificirani v razred uˇciteljev
in razred ˇstudentov. Asistenti so poseben pod-razred uˇciteljev in ˇstudentov, ki
imajo lastnosti obeh.
1. Definiraj razred oseba ki vsebuje podatke o imenu, priimku in identifika-
torju osebe. Razred oseba implementira metodo predstavi.*)


class oseba vpisna_st name surname =
  object (main)
    val mutable id =(vpisna_st:int) 
    val mutable ime = (name:string) 
    val mutable priimek = (surname:string)
    method predstavi =ime^" "^priimek^ " "^string_of_int(id)
end;;

class ucitelj vpisna_st name surname placa = 
  object 
  inherit oseba vpisna_st name surname as main
  val mutable placa = (placa:int)
  method predstavi = main#predstavi ^" in imam plačo:  "^string_of_int(placa)
end;;

class student vpisna_st name surname mentor = 
  object 
  inherit oseba vpisna_st name surname as main
  val mentor = (mentor:string)
  method predstavi = main#predstavi ^" "^ mentor
end;;

class asistent vpisna_st name surname placa mentor = 
  object (self)
  inherit oseba vpisna_st name surname as oseba
  inherit ucitelj vpisna_st name surname placa as ucitelj
  inherit student vpisna_st name surname mentor as student
  method predstavi = oseba#predstavi ^ ucitelj#predstavi
  method predstavi_vse = print_string(oseba#predstavi^"  "^ucitelj#predstavi^"  "^student#predstavi)
end;;


let oseba1 = new oseba 89191228 "Ana" "Hojan";;
