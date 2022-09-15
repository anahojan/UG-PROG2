type zaposleni = Natakar of string | Cistilec of string | Kuhar of string;;


class it_rest (p1:int) p2 kuhar (naslov:string) =
	object
		val mutable mize = p1
		val mutable ocena = (p2:float)
		val kuhar = (kuhar:string)
		val mutable osebje = ([]:zaposleni list)
		val naslov = naslov
		method st_osebja = List.length osebje
		method get_mize = mize
		method get_ocena = ocena
		method get_kuhar = kuhar
		method private get_osebje = osebje
		method get_naslov = naslov
		
	end;;

let forte = new it_rest 15 4.34 "Genarro Botticelli" "Via del Pomodore 13";;

forte#st_osebja
forte#get_kuhar
forte#get_osebje
