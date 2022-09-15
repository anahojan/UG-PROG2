(*	algoritmi urejanja 	*)

(* Stalin sort *)

let rec find_min sez = match sez with 
| [x] 	-> x,[]
| g::r  -> let (m,p)=find_min r in if g<m then g,m::p
 																					else m,g::p
| []    -> 0,[];;

find_min [154;8;97;-585;45;0];;

let stalin_sort = function
	| [] 		-> []
	| sez -> let (m,p) = find_min sez in m::stalin_sort p;;

let stalin_sort sez = match sez with
	| [] -> []
	| _  -> let (m,p) = find_min sez in m::stalin_sort p;;

stalin_sort [154;8;97;-585;45;0];;

(* selection sort *)
let rec insert x sez = match sez with 
| [] -> [x]
| g::_ when x<g -> x::sez
| g::r -> g::insert x r;;

insert 50 [1;5;16;43;61;76;99];;

let rec insertion_sort1 sorted unsorted = match unsorted with
| []  -> sorted
| g::r-> insertion_sort1 (insert g sorted) r;;

let insertion_sort = insertion_sort1 [];; 

(*---- insertion sort kot ena funkcija ---*)

let insertion_sort = 
	let rec insert x sez = match sez with 
| [] -> [x]
| g::_ when x<g -> x::sez
| g::r -> g::insert x r
	in 
	let rec insertion_sort1 sorted unsorted = match unsorted with
  | []  -> sorted
  | g::r-> insertion_sort1 (insert g sorted) r
	in insertion_sort1 [];; 
	
(*				MERGE SORT				*)

let rec split s1 s2 seznam = match seznam with 
| [] 	 -> s1,s2
| g::r -> split s2 (g::s1) r;;
		
		
split [] [] [154;8;97;-585;45;0];;


let rec merge = function
	| [],s 					-> s
	| s,[] 					-> s
	| g1::r1,g2::r2 -> if g1<g2 then g1::(merge (r1,(g2::r2)))
															else g2::(merge (r2,(g1::r1)));;
	
merge ([1;46;87;100],[-45;0;68;88;109]);;


	
let rec merge_sort seznam = match seznam with 
| [] | [_] -> seznam
| _  -> let s1,s2 = (split [] [] seznam) in 
		merge (merge_sort s1,merge_sort s2);;
	
merge_sort [154;8;97;-585;45;0];;

(*quicksort*)

let rec pivot_split sez x = 
	List.filter (fun y -> y<x) sez,List.filter (fun y -> y>=x) sez
	
let rec quick_sort seznam = match seznam with 
| []   -> []
| g::r -> let s1,s2=pivot_split r g in
						(quick_sort s1) @ [g] @ (quick_sort s2);;

quick_sort	[154;8;97;-585;45;0];;