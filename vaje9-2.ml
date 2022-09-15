type kosi = Mesano of int | Otok of int | Kuzki 

let rec v_o k = match k with 
			| [] 		 -> 0
			| Mesano a ::tl -> a   + v_o tl
			| Otok a :: tl  -> 5*a + v_o tl
			| Kuzki ::  tl  -> v_o tl

class ['tip] park (d:int) (k:int) (p:int) (v:'tip) =
	object (self)
		val mutable drevesa = d
		val mutable klopce = k
		val povrsina = p
		val mutable kosi = ([]:kosi list)
		val vsebina = v
		method get_drevesa = drevesa
		method get_klopce = klopce
		method get_seznam_kosev = kosi
		method private set_drevesa p = drevesa <- p 
		method private set_klopce p = klopce <- p 
		method private set_kosi p = kosi <- p
		method dodaj_kos k = kosi <- (k :: kosi)
		method dodaj_drevo = drevesa <- drevesa+1
		method odstrani_drevo = 
			self#set_drevesa (drevesa-1);
			self#set_klopce (klopce+1)
		method odstrani_klopco = 
			self#set_drevesa (drevesa+1);
			self#set_klopce (klopce-1)
		method get_kosi = let rec v_o k = match k with 
			| [] 		 -> 0
			| Mesano a ::tl -> a   + v_o tl
			| Otok a :: tl  -> 5*a + v_o tl
			| Kuzki ::  tl  -> v_o tl
			in v_o kosi
	end
	
let park1 = new park 15 20 150 Kuzki;;
let park2 = new park 11 210 1150 "Velik park";;

let vec_sedisc p1 p2 = 
	if p1#get_klopce>p2#get_klopce 
	then print_string "prvi park ima vec sedisc"
	else print_string "drugi park ima vec sedisc"

park1#dodaj_kos Kuzki
park1#dodaj_kos (Mesano 20)
park1#dodaj_kos (Otok 30)
park1#get_kosi
park1#get_seznam_kosev	

park1

