module Modul1 = 
	struct
    (*modul1.ml*)
    let povecaj x = x+1;;
    let kvadrat x = x*x;;
    let pomanjsaj x = x-1;;
    let kub x = x*x*x;;
	end
	
module Modul2 = 
	struct
		(*modul2.ml*)
		let init () = print_int (Modul1.povecaj(Modul1.kub 5));;
	end
	
module type MODUL1 = 
  sig
    val povecaj : int -> int
    val kvadrat : int -> int
  end

module NovModul = (Modul1:MODUL1);;

Modul1.kvadrat 5;;
Modul2.init ()

NovModul.kub 5



module MaxHeap = 
	struct
		(*find-max (or find-min): find a maximum item of a max-heap, *)
		(* or a minimum item of a min-heap, respectively (a.k.a. peek)*)
		let find_max heap = Array.get heap 0;;
		(*create-heap: create an empty heap*)
		let create_heap = [||];;
		let delete_max heap = 
			let delete_last heap = Array.sub heap 0 (size heap -1)  in
			let last = Array.get heap (size heap -1) in
			heap.(0)<- last; let r = delete_last heap in
			shift_down r 0;;

		let size heap = Array.length heap;;
		
		let max_index heap i j = if Array.get heap i >= Array.get heap j 
															then (i,j) else (j,i);;
			
		let swap heap i j = 
			let (vi,vj) = (Array.get heap i),(Array.get heap j) in 
			heap.(i) <- vj; heap.(j) <- vi; heap;; 
			
		let rec shift_down heap i = 
			let l_c = (2*i)+1 in
			if size heap <= l_c then heap else
				let (c1,c2) = if (size heap) =l_c+1 then (l_c,l_c) else 
					max_index heap (l_c) (l_c+1) in 
				match max_index heap i c1 with
				| (x,_) when x=i -> heap
				| _ -> shift_down (swap heap i c1) c1;;
	end
	
	
let h1 = [|100;19;36;17;3;25;1;2;7|];;
swap h1 1 3
shift_down h1 0

delete_max h1

module type MAXHEAP =
	sig
    val find_max : 'a array -> 'a
    val create_heap : 'a array
    val delete_max : 'a array -> 'a array
    val size : 'a array -> int
  end
