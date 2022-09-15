class kosara (* morebitni parametri *) = 
	object 
		(*metode, atributi*)
		val mutable jabolka = 0
		val mutable hruske = 0
		method get_jabolka = jabolka
		method get_hruske = hruske
		method set_jabolka a = jabolka<-a
		method set_hruske a = hruske<-a
	end;;

let k1 = new kosara;;
k1#set_hruske 12;;
k1#get_hruske;;

(* razred hisa *)
class hisa barva p2 p3 p4 p5 p6 =
	object
	val mutable barva = (barva:string)
	val mutable st_oken = (p2:int)
	val mutable st_vrat = p3
	val st_etaz = p4
	val visina_etaze = 2.55
	val st_sob = p5
	val velikost_sobe = p6
	
	method get_barva = barva
	method get_st_oken = st_oken
	method visina = float_of_int(st_etaz)*.visina_etaze
	method kvadratura = st_sob*velikost_sobe
	end;;
	
let hisa1 = new hisa "oranzna" 12 9 3 11 12;;
let hisa2 = new hisa "crna"    9  7 4 16 7;;

let boljsa h1 h2 = 
	if h1#visina>h2#visina && h1#kvadratura>h2#kvadratura
	then print_string "Prva hisa je boljsa od druge."
	else if h1#visina<h2#visina && h1#kvadratura<h2#kvadratura
	then print_string "Prva hisa je slabsa od druge."
	else print_string "Hisi sta neprimerljivi.";;
	
boljsa  hisa2 hisa1;;
	
float_of_int(hisa1#kvadratura)*.19.5;;


class prevozno_sredstvo p1 p2 p3=
	object 
	val mutable hitrost = (p1:float)
	val mutable tip_transporta = (p2:string)
	
	method get_hitrost = hitrost
	method get_tip_transporta = tip_transporta
	method set_hitrost a = hitrost<-a
	method set_tip_transporta a = tip_transporta<-a
	method cas_do_faksa oddaljenost_od_faksa = (oddaljenost_od_faksa/.hitrost)*.60.0
	end;;

let avto1 = new prevozno_sredstvo 60. "avto" 12.3;;

avto1#cas_do_faksa 12.3;;


class ['a] hisa barva p2 p3 p4 p5 p6 (zaklad:'a) =
	object (self)
	initializer if p2< p5 then print_string "Opozorilo: premalo oken!" else ()
	val mutable barva = (barva:string)
	val mutable st_oken = (p2:int)
	val mutable st_vrat = p3
	val st_etaz = p4
	val visina_etaze = 2.55
	val mutable st_sob = p5
	val velikost_sobe = p6
	val zaklad = zaklad
	
	method get_barva = barva
	method get_st_oken = st_oken
	method visina = float_of_int(st_etaz)*.visina_etaze
	method kvadratura = st_sob*velikost_sobe
	method private set_st_oken a =  st_oken <- a
	method private set_st_vrat a =  st_vrat <- a
	method vrata_v_okno = 
		self#set_st_oken (st_oken+1);
		self#set_st_vrat (st_vrat-1)
	method okno_v_vrata = 
		self#set_st_oken (st_oken-1);
		self#set_st_vrat (st_vrat+1)
	method get_zaklad = zaklad
	end;;

let hisa1 = new hisa "oranzna" 1 6 3 11 12;;
let hisa2 = new hisa "crna"    9  7 4 16 7;;

