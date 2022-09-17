(*1 Okuˇzba

(b) Napiˇsi funkcijo onstepcontagion : bool array array -> bool array
array = <fun> ki kot vhod sprejme matriko tipa bool, kjer vrednosti true
predstavljajo okuˇzenih kvadratke, vrednosti false pa neokuˇzene. Funkcija
naj izraˇcuna kako se okuˇzba v enem koraku razˇsiri, in sicer po naslednjem
pravilu:
• okuˇzeni kvadratki ostanejo okuˇzeni za vedno
• neokuˇzeni kvadratki postanejo okuˇzeni, ˇce vertikalno oz. horizontalno
mejijo z vsaj dvema okuˇzenima kvadratkoma.*)





(*1.*)
let matrix=[|[|false;true;false|];
              [|true;false;false|]; 
              [|false;false;true|];
              [|false;false;false|]|];;

let matrix2=[|[|false; false; false; false; true; false; false|];
  [|false; false; false; true; false; false; false|];
  [|true; false; false; false; false; false; false|];
  [|false; true; false; false; false; false; false|];
  [|false; false; true; false; false; false; false|];
  [|false; false; false; false; false; false; true|];
  [|false; false; false; false; false; true; false|]|];;
  
let matrix3=[|[|false; true; true|]; [|false; false; true|]; [|false; false; false|];
  [|false; true; false|]; [|false; true; false|]; [|true; false; false|]|];;

let  printmat m = 
for i = 0 to Array.length m-1 do
  for j = 0 to Array.length m.(i)-1 do 
  if m.(i).(j) == true then print_int 1 else print_int 0;
  if j == Array.length m.(i)-1 then print_string "\n"
  done
done;;

(*dela pravilno*)
let onstepcontagion m = 
  let kopija =  Array.(map copy ) m in
  let counter = ref 0 in 
  for i = 0 to  Array.length kopija-1 do
    for j = 0 to Array.length kopija.(i)-1 do 
      counter:=0;
      if (kopija.(i).(j)=false) then 
        (if (j+1 <=  Array.length kopija.(i)-1) then
          if (kopija.(i).(j+1)=true) then counter := !counter +1 ;
        if (j-1 >= 0 ) then
          if (kopija.(i).(j-1)=true) then counter := !counter +1 ;
        if (i+1 <= Array.length kopija-1) then
          if (kopija.(i+1).(j)=true) then counter := !counter +1 ;
        if (i-1 >= 0) then
          if (kopija.(i-1).(j) = true) then counter := !counter +1; 
        if (!counter >=2) then (m.(i).(j) <- true) else m.(i).(j)<-false;
        counter:= 0;
        ) else counter:=0;
    done;
    counter := 0; 
  done;
  m
;;

(*2. *)
type player= Orange | Red | Blue | White ;;
type piece= Knight of player| Town of player | City of player | Road of player | Wool of player |  Brick of player | Lumber of player | Grain of player | Ore of player;;

let posessions=[|Ore Orange;Road Red;Ore Blue;Ore White;Lumber White;Ore Red;Knight Red;Road Orange;Lumber Red;Brick Red;City White;Lumber White;Wool Red;City White;Ore White;Brick White;Road Blue;Lumber Blue;Grain Orange;Wool Red;Road White;Knight White;Grain White;Wool Orange;City Blue;Ore Orange;Knight Orange;Brick White;Ore Red;Ore White;Road Orange;Knight Red;Lumber Orange;Wool Orange;City Orange;Wool Blue;Lumber White;City Red;Grain Red;Lumber Red;Ore White;Grain White;Brick Orange;Brick White;Road Blue;Grain Red;Ore White;City White;Road White;Knight Orange;Brick Red;Ore White;Lumber Orange;Ore Blue;Road Blue;Brick White;Brick Orange;Ore Orange;Ore Blue;Ore Orange;Ore Orange;Brick Orange;Brick White;Road Orange;Lumber White;Knight Red;Brick White;Brick Orange;Road Blue;Brick Orange;Lumber Blue;Road Blue;Ore Red;Grain Blue;Wool Red;Town Red;Ore Orange;Lumber White;Road Red;Road Blue;Town Blue;Wool Red;Lumber Blue;Wool Blue;Town Red;Grain Orange;Brick Blue;Knight Red;Wool White;Ore Orange;Ore Blue;Lumber White;Wool White;Brick Orange;Ore Blue;Grain Red;Wool Blue;Road Red;Ore Blue;Lumber Orange|];;

let posessions2=
[|Ore Blue;City Orange;Grain Blue;Road Red;Brick Orange;Road Orange;Ore Orange;Wool Blue;Wool Blue;Wool Red;Brick Red;City Blue;Town Blue;Brick Blue;City Blue;Lumber Orange;City Blue;Town Red;Wool White;Brick Blue;Road White;Ore Orange;Town White;Road Orange;Brick Blue;Brick White;Ore White;Ore White;Ore Blue;Road White;Lumber Orange;Road Red;Brick Orange;Ore Red;Ore Blue;Lumber Red;Lumber White;Ore White;Grain Red;Lumber Red;Brick Blue;Lumber White;Road Blue;Wool Orange;Wool Red;Lumber Orange;Wool Red;Lumber Orange;Brick White;Grain White;Road Blue;Wool Orange;Lumber White;Lumber Orange;Wool Orange;Brick Orange;Wool White;Grain Red;Ore Blue;Ore Red;Ore Orange;Wool Blue;Brick Orange;Grain Orange;Wool Orange;Town White;City Orange;Knight Red;Road Blue;City Blue;Lumber White;Brick Red;Wool Red;Town White;Grain White;Knight Blue;Road Orange;City Red;Grain White;Ore Orange;Road Blue;Grain Orange;Road Blue;Grain Orange;Knight Orange;Grain White;Road Blue;Ore Orange;Road White;Lumber White;City Orange;Lumber White;Brick Blue;Ore Red;Lumber Blue;Road Orange;Ore Orange;Road Red;Town Orange;Lumber White;Grain Red;Ore Blue;Wool White;Road Orange;Wool Orange;Town Blue;Road Blue;Road Red;Brick Orange;Lumber Orange;Road White;|];;

let posessions3=
[|Ore White;Brick Orange;Ore Blue;Brick Red;Lumber White;City Blue;Grain Orange;Town Orange;Lumber Red;Grain Red;Ore Red;Ore Red;Lumber White;Town Orange;Lumber Orange;Knight Blue;Road Blue;Knight White;Road Blue;Lumber White;Wool Orange;Ore White;Lumber Orange;Knight White;Brick White;City Red;Knight Blue;Road White;City Orange;Road Blue;Brick Orange;Brick Orange;Brick White;Ore Blue;Grain Red;City White;Grain White;Grain Orange;Road Blue;Lumber Red;Wool Blue;Road Orange;Lumber Orange;Town White;Knight Orange;Grain White;Lumber White;Town Red;Town Blue;Grain Blue;Road White;Lumber White;Road Blue;Lumber Red;Town Red;Knight Blue;Grain Red;Ore Orange;Wool White;Wool Blue;Brick Orange;Grain Orange;Grain Red;Knight Red;Road Red;Road Orange;Ore Blue;Brick Blue;Grain Red;Grain Blue;Wool White;Lumber Red;Knight Red;Road Orange;Wool Red;City Orange;Grain Red;Brick Orange;Road White;Brick Red;Brick White;Lumber Orange;Town Blue;Ore White;Wool White;Road Blue;Brick White;Lumber Blue;Brick Blue;Lumber Orange;Road Blue;Town Blue;Lumber Orange;City Red;Road Blue;Knight Blue;Wool Blue;City Red;Wool Red;Grain White;Brick White;Lumber Orange;Road White;Town Red;Wool Red;Ore Red;Ore White;Ore Red;Knight Orange;Lumber White;Road White;Lumber Red;Wool Blue;Road Orange;Town Orange;Ore Red;Grain White;Brick Blue;Grain Orange;Road White;City White;Knight White;Grain White;Grain Blue;Ore Orange;Road White;Ore White;Knight Red;Wool Orange;Wool Red;Wool White;City Orange;City Blue;Ore Red;City Blue;Brick White;Grain White;Lumber White;Road Red;Ore Red;Road Orange;Lumber Blue;Brick Orange;Grain Red;Road Orange;Brick Red;Knight Red;Wool Orange;Road Blue;Brick Blue;Road Orange;Town White;Lumber White;Knight Orange;Wool White;Grain White;Town Blue;Brick White;Wool Orange;Road Red;Brick Blue;Ore Blue;Wool Blue;Lumber Orange;Brick Blue;Road Blue;City Orange;Town Blue;Town Red;City Orange;Wool Blue;Ore Red;Grain Red;Brick Blue;Ore White;Wool Red;Brick Blue;Road White;Brick Red;Brick White;Brick White;Lumber Orange;Wool Red;Wool Orange;Wool White;Lumber White;Road Blue;City White;Wool Red;Wool Orange;Ore White;Road Red;Road White;Lumber Blue;Ore Red;Brick Red;Lumber Red;Lumber Blue;Wool Orange;Road Blue;|];;

let posessions4=
[|Wool White;Grain White;Grain Blue;Brick Orange;Lumber Red;Brick Orange;Grain White;Grain Blue;Wool Orange;Lumber White;Road Red;Wool Red;Brick Red;Lumber Red;Grain Blue;Grain Orange;Lumber Blue;Town White;Road Red;Ore Blue;City Red;Grain Orange;Town Red;Lumber White;Wool Blue;Grain Orange;Road Orange;Lumber White;Brick Blue;Ore Orange;Knight Blue;Road Blue;City Blue;Grain Red;Wool Blue;Lumber Orange;Brick Red;Wool Red;Brick Orange;Wool Red;Ore Red;Lumber Red;Lumber Red;Ore White;Grain Red;City White;Town White;Wool Red;Wool Red;Brick Red;Ore Red;Knight White;Wool Red;Ore Red;Road Blue;Lumber Red;Ore White;Brick White;Town Blue;Grain Red;Wool Blue;Brick Red;Lumber White;Road Orange;Wool Orange;Lumber Blue;Brick Red;Grain White;Wool White;Lumber Red;Lumber White;Wool Red;Knight Red;Ore Blue;Lumber White;Ore Orange;Wool Red;Brick White;Grain White;Town White;Road Orange;Town White;Wool Red;Lumber White;Brick White;City Orange;Lumber Red;Grain White;Wool Blue;Grain Red;Grain Blue;Wool Blue;Grain Red;Ore Orange;Lumber Red;Ore Blue;Town Red;Road Blue;Knight Blue;Lumber Red;Town White;Grain Blue;Brick White;Brick White;City Blue;Ore Orange;Wool Orange;Grain Blue;City Orange;Knight Red;Ore White;Town Orange;Brick Orange;Knight Orange;Knight Blue;City White;Grain Orange;Brick Blue;Brick Red;City Red;Knight Red;Wool Red;Knight White;Wool Red;Lumber Blue;City Blue;Ore Blue;Town Blue;Wool Orange;Road White;Ore Orange;City Red;Brick White;Ore Blue;Wool Orange;Wool Red;Knight Orange;Road Blue;Brick White;Ore Blue;Grain White;Ore Orange;Wool Red;Wool Orange;Town White;Road Red;Brick White;Lumber White;Ore Orange;Grain Blue;Road Red;Lumber Orange;Brick White;Grain White;Lumber Red;Brick Red;Road Red;Wool White;Wool Blue;Knight Red;Lumber Blue;Brick Blue;Grain Blue;Ore Orange;Grain White;Town Blue;Grain Blue;Wool White;Wool Orange;City Orange;Lumber Orange;City White;Lumber Blue;Brick Red;Brick Orange;Wool Blue;Brick Red;Town Blue;Road Red;Brick White;Road Orange;Town Orange;Road White;Brick Red;Grain Orange;Brick Red;Brick Red;Lumber White;Lumber Orange;Lumber Orange;City Orange;Brick White;Grain White;Knight White;City Blue;Wool Blue;Brick Orange;Brick White;Knight Red;Ore Orange;Road White;Ore White;City Blue;Ore Red;Wool White;Brick Red;Wool Red;City Orange;Wool Blue;Grain Blue;Road Blue;Lumber Red;Road Orange;Ore Red;Ore Red;City White;Brick Red;Grain Red;Road Blue;Brick White;Wool White;Town Orange;Grain White;Road Orange;Grain Orange;Ore Blue;Lumber White;Lumber White;Ore White;Lumber White;Wool White;Lumber Orange;Lumber Blue;Knight Red;Brick White;Knight Orange;Road Red;Grain Red;Ore Red;Road Blue;Wool Orange;Lumber Blue;Wool Orange;Knight Orange;Brick White;Knight Blue;Grain Orange;Road Orange;Road Orange;Lumber Orange;Knight Orange;Brick Red;Brick Blue;Ore Red;Road Red;City Red;Lumber White;Road Orange;Road White;Lumber White;Ore Blue;Wool Orange;Lumber Orange;City White;Road Red;Brick Orange;Road Blue;City White;Wool Orange;Town Red;Ore Orange;Town Red;Ore Orange;Ore White;City Orange;Wool Red;Lumber Orange;Ore Blue;Road Blue;Lumber White;Town Blue;Road Blue;Wool White;Brick White;Ore Orange;Grain White;Town Orange;Brick Red;Brick Red;Knight Orange;Grain Red;Grain White;Lumber Orange;Lumber Orange;Wool Blue;Road Orange;Town White;Brick Blue;Grain Orange;Grain Blue;|];;


let getplayer igralec = match igralec with
  | Knight Blue -> Blue
  | Knight Red -> Red
  | Knight Orange -> Orange
  | Knight White -> White
  | Town Blue-> Blue
  | Town Red-> Red
  | Town Orange-> Orange
  | Town White-> White
  | City Blue -> Blue
  | City Red -> Red
  | City Orange -> Orange
  | City White -> White
  | Road Blue -> Blue
  | Road Red -> Red
  | Road Orange -> Orange
  | Road White -> White
  | Wool Blue -> Blue
  | Wool Red -> Red
  | Wool Orange -> Orange
  | Wool White -> White
  | Brick Blue -> Blue
  | Brick Red -> Red
  | Brick Orange -> Orange
  | Brick White -> White
  | Lumber Blue -> Blue
  | Lumber Red -> Red
  | Lumber Orange -> Orange
  | Lumber White -> White
  | Grain Blue -> Blue
  | Grain Red -> Red
  | Grain Orange -> Orange
  | Grain White -> White
  | Ore Blue -> Blue
  | Ore Red -> Red
  | Ore Orange -> Orange
  | Ore White -> White ;;


  (*to dela*)
  let filterbyplayer barva polje_figur = 
    let list_of_poljefigur = Array.to_list (polje_figur) in
    let polje = Array.of_list(List.filter(fun x-> (getplayer x == barva)) list_of_poljefigur) in
    
    polje
;;

(*to dela*)
let countpoints barva polje_figur = 
  let count = ref 0 in
  let st_road = ref 0 in 
  let st_knight = ref 0 in
  let poljefigur_na_igralca = (filterbyplayer barva polje_figur) in 
  for i = 0 to Array.length poljefigur_na_igralca -1 do
    if (poljefigur_na_igralca.(i) = Town barva) then count := !count + 1 ;
    if (poljefigur_na_igralca.(i)= City barva) then count := !count +2;
    if (poljefigur_na_igralca.(i)= Road barva) then if (!st_road = 5 ) then count := !count +2 else
      (st_road := !st_road + 1);
    if (poljefigur_na_igralca.(i)=Knight barva) then if (!st_knight = 3 ) then count := !count +2 else
      (st_knight := !st_knight + 1);
  done;
!count;;








(*3. Vodovodni sistem*)
type pipe= Plus | Minus | Block | Midbar;;

let pipeline1 =[|
 [| Minus ; Plus ; Minus ; Midbar ; Plus |];
 [| Midbar ; Plus ; Plus ; Minus ; Midbar |];
 [| Minus ; Plus ; Block ; Plus ; Block |];
 [| Plus ; Plus ; Minus ; Minus ; Block |];
 [| Minus ; Block ; Plus ; Midbar ; Block |];|];;

let pipeline2 =[| 
[| Plus ; Minus ; Plus|];
[| Minus ; Minus ; Midbar|];
[| Plus ; Minus ; Plus|];
[| Plus ; Plus ; Block|];|];;

(*to dela*)
let print_pipeline m1 m2  = 
for i = 0 to Array.length m2 -1 do
  for j = 0 to Array.length m2.(i)-1 do
    if (m2.(i).(j) == false) then print_string " " else 
      if (m1.(i).(j) == Plus ) then print_string "+" ;
      if (m1.(i).(j)== Minus) then print_string "-";
      if (m1.(i).(j)== Midbar) then print_string "|";
      if (m1.(i).(j)== Block) then print_string "x";

      if j=Array.length m2.(i)-1 then  print_string "\n"
  done
done
;;

  let alltrue1 = Array . map ( fun x -> Array . map ( fun y -> true ) x ) pipeline1;;

  (*to dela*)
  let rotate_pipeline m1= 
  let count = ref 0 in 
  for i = 0 to Array.length m1 -1 do
    for j = 0 to Array.length m1.(i)-1 do
      count:= 0;
      if (m1.(i).(j)= Midbar && !count = 0) then (
        count:= 1;
        m1.(i).(j) <- Minus ;);
      if (m1.(i).(j)= Minus && !count = 0 ) then (
      count:=  1;
      m1.(i).(j)<- Midbar;);
    done;
    count:= 0;
  done;
  m1;;

  (*ne dela pravilno*)



let explore_pipeline m = 
  let i_size = Array.length m  in 
  let j_size = Array.length m.(1)   in 
  let bool_polje = (Array.make_matrix i_size j_size false) in
  let check_matrix = (Array.make_matrix i_size j_size false) in
  
  let rec rekurzija i j from = 
    if (i >= 0 && j >=0 && i < i_size && j< j_size ) then (
      if(check_matrix.(i).(j) == false ) then (
        check_matrix.(i).(j) <- true; 
        match m.(i).(j) with 
        | Plus -> bool_polje.(i).(j)<-true; rekurzija (i+1) j "updown"; rekurzija i (j+1) "side" ; rekurzija (i-1) j "updown"; rekurzija i (j-1) "side";
        | Midbar when from = "updown" -> bool_polje.(i).(j)<-true; rekurzija (i+1) j "updown"; rekurzija (i-1) j "updown";
        | Minus when from = "side" -> bool_polje.(i).(j)<-true; rekurzija i (j+1) "side"; rekurzija i (j-1) "side";
        | _ -> bool_polje.(i).(j)<-false;
      );
    ) in (
      match m.(0).(0) with
        | Plus -> rekurzija 0 0 "side" ; rekurzija 0 0 "updown"
        | Midbar -> rekurzija 0 0 "updown"
        | Minus -> rekurzija 0 0 "side"
        | Block -> Unit.(())

    );
    bool_polje;;



  


      
  