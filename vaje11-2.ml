module Modul1 = 
	struct
		(*modul1.ml*)
    let povecaj x = x+1;;
    let kvadrat x = x*x;;
    let pomanjsaj x = x-1;;
    let kub x = x*x*x;;
	end
	
module type MODUL1 = 
	sig
    val povecaj : int -> int
    val kub : int -> int
  end;;
	
module MojModul = (Modul1:MODUL1);;

Modul1.pomanjsaj 105;;
MojModul.pomanjsaj 105;;


module MaxHeap = 
	struct
		let size heap  = Array.length heap;;
		let find_max heap = Array.get heap 0;;
		let swap heap i j = 
			let vi,vj = (Array.get heap i), (Array.get heap j) in
			heap.(i)<-vj; heap.(j)<-vi; heap;;
		let max_index heap i j = 
			if Array.get heap i > Array.get heap j
			then (i,j) else (j,i)
		let rec shift_up heap i =
			let s = (i-1)/2 in
			match max_index heap s i with
			| (_,x) when x=i -> heap 
			| _ -> shift_up (swap heap s i) s;;
		let rec shift_down heap i = 
			let l_c = 2*i+1 in
			if size heap <= l_c then heap else
				let (c1,c2) = if size heap = l_c+1 then (l_c,l_c) 
				else max_index heap l_c (l_c+1) in
				match max_index heap i c1 with
				| (x,_) when x=i -> heap
				| _-> shift_down (swap heap i c1) c1;;
		let delete_last heap = Array.sub heap 0 (size heap - 1);;
		let delete heap i = 
			let last = Array.get heap (size heap -1) in 
			let novaKopica = delete_last heap in
			novaKopica.(i)<- last; shift_down novaKopica i;;
		let delete_max heap = delete heap 0;;
	end

module type MAXHEAP = 
  sig
    val size : 'a array -> int
    val find_max : 'a array -> 'a
		val delete_max : 'a array -> 'a array
    val delete : 'a array -> int -> 'a array
  end
	
let h1 = [|100;19;36;17;3;25;1;2;7|];;
MaxHeap.swap h1 0 4;;
MaxHeap.max_index h1 1 2;;

MaxHeap.shift_up [|100;19;36;17;3;25;1;2;70|] 8;;
MaxHeap.shift_down [|10;19;36;17;3;25;1;2;7|] 0;;
MaxHeap.delete_max h1
