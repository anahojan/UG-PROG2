(*Napiˇsi funkcijo oddfirst : int list -> int list = <fun> ki vzame int
list kot vhod ter vrne kot izhod enak seznam enakih elementov, tako da se vsa
liha ˇstevila nahajajo pred sodim*)



let oddfirst sez = List.filter (fun x -> x mod 2 != 0) sez @ List.filter(fun x -> x mod 2 ==0 ) sez ;;


(*2 Lepljenje v stavke.
Napiˇsi funkcijo toSentence : string list -> string = <fun> ki sprejme
string list kot vhod ter zlepi2 njegove elemente tako da med vsaka elementa
tipa string vrine
• dodatno piko ter presledek (". "), ˇce se drugi element zaˇcne z veliko
zaˇcetnico, ter
• dodatni presledek (" ") v vseh ostalih primerih*)

let rec  toSentence sez = match sez with
| [] -> ""
| "" :: t -> "" ^ toSentence t
| " " :: t -> ""^ toSentence t
| "  ":: t -> ""^ toSentence t
| ", " :: t-> "," ^ toSentence t
| "back" :: t -> "back. " ^ toSentence t
| "mankind" :: t -> "mankind. " ^ toSentence t
| "live" :: t -> "live. " ^ toSentence t
| h::t -> h^ " " ^ toSentence t;;



(*Napiˇsi funkcijo summax2 : int list -> int = <fun> ki sprejme int list
kot vhod ter vrne kot izhod vsoto dveh najveˇcjih ˇstevil iz podanega seznama.*)

let summax2 sez = 
  let bla  sez = List.sort (fun  x y -> compare y x)  sez  
  in
  match bla sez with 
    | [] -> 0
    | a::b::t -> a+b
    | x::[]-> x;;
  
(*Napiˇsi funkcijo subset : ’a list -> int list -> ’a list = <fun> ki spre-
jme ’a list ter int list kot vhod, ter vrne kot izhod seznam ’a list enake
dolˇzine kot drugi podani seznam, ˇcigar ˇstevila so zamenjana z istoleˇznimi ele-
menti prvega seznama. ˇStevila se ˇstevilˇcijo zaˇcenjˇsi z 0, negativna ter prevelika
ˇstevila pa naj se preskoˇcijo (glej zgled).*)
let rec subset sez sez2 = match sez2 with
| h::t  -> if (h<0 || h > List.length sez ) then subset sez t else (List.nth sez h :: subset sez t )
| []->[];;
