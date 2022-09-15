(*	Algoritmi za urejanje	  *)

(*	Stalin sort							*)

let rec find_min sez = match sez with
| []  -> (0,[])
| [m] -> (m,[])
| g::r-> let (m,p) = find_min r in
					if g<m then g,m::p
								 else m,g::p;;

find_min [-9;548;4;78;-66];;

let rec stalin_sort sez = match sez with
| [] -> sez
| _ -> let (m,p) = find_min sez in m::stalin_sort p;;

stalin_sort [-9;548;4;78;-66];;

(*--------------------------------*)

let rec find_min sez = match sez with
| []  -> (0,[])
| [m] -> (m,[])
| g::r-> let (m,p) = find_min r in
					if g<m then g,m::p
								 else m,g::p;;

let rec stalin_sort sez = 
	let rec find_min sez = match sez with
    | []  -> (0,[])
    | [m] -> (m,[])
    | g::r-> let (m,p) = find_min r in
    					if g<m then g,m::p
    								 else m,g::p 
	in
	match sez with
  | [] -> sez
  | _ -> let (m,p) = find_min sez in m::stalin_sort p;;

(*		sortiranje z vstavljanjem		*)

let rec insert sez x = match sez with 
| [] -> [x]
| g::_ when x<g -> x::sez
| g::r -> g::insert r x;;

insert [-66; -9; 4; 78; 548] (-254);;
let rec insertion_sort sorted unsorted = match unsorted with
| [] 	 -> sorted 
| g::r -> insertion_sort (insert sorted g) r;;


(* merge sort	*)

let rec split sez = match sez with
| [] | [_]  -> sez,[]
| g1::g2::r -> let r1,r2 = split r 
	in
	g1::r1,g2::r2;;
 
split [-254; -66; -9; 4; 78; 548];;

let rec split1 v1 v2 sez = match sez with 
| [] 	 -> v1,v2
| g::r -> split1 v2 (g::v1) r;;

split1 [] [] [-254; -66; -9; 4; 78; 548];;

let rec merge sez1 sez2 = match sez1,sez2 with
| [],_ 					-> sez2
| _,[] 					-> sez1
| g1::r1,g2::r2 -> if g1<g2 then g1::merge r1 sez2
														else g2::merge sez1 r2;;

merge [-8;8;88;790] [-548;0;15;76];;

let rec merge_sort seznam = match seznam with 
| [] | [_] -> seznam
| _ -> let (s1,s2) = split seznam in 
				let (s3,s4) = (merge_sort s1,merge_sort s2) in
				 merge s3 s4;;

merge_sort [-8;8;88;790;-548;0;15;76];;

(*	quicksort 	*)
let pivot_split seznam x = 
	List.filter (fun y-> y<x) seznam, List.filter (fun y-> y>=x) seznam;;

let rec pivot_split sez x = match sez with 
| [] -> [],[]
| g::r -> let s1,s2 = pivot_split r x in if g<x then g::s1,s2 else s1,g::s2;;

pivot_split [-8;8;88;790;-548;0;15;76] 0;;

let rec quick_sort seznam = match seznam with 
| []   -> []
| g::r -> let s1,s2 = pivot_split r g in
						(quick_sort s1) @ [g] @ (quick_sort s2);;

quick_sort [-8;8;88;790;-548;0;15;76];;

