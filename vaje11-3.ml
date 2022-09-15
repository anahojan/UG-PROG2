module Modul1 = 
	struct
    (*modul1.ml*)
    let povecaj x = x+1;;
    let kvadrat x = x*x;;
    let pomanjsaj x = x-1;;
    let kub x = x*x*x;;
	end;;

module type MODUL1 = 
  sig
    val povecaj : int -> int
    val kvadrat : int -> int
  end;;
	
module Jst = (Modul1:MODUL1);;
Jst.povecaj 123;;
Jst.kub 123;;

module MaxHeap =
	struct
		let size h = Array.length h;;
		let find_max h = h.(0);;
		let swap h i j = 
			let (vi,vj) = h.(i),h.(j) in 
				h.(i)<-vj; h.(j)<-vi; h;; 
		let max_index h i j = 
			if h.(i)>h.(j) then i else j;;
		let rec shift_up h i = 
			let p = (i-1)/2 in if max_index h i p = p
			then h else shift_up (swap h i p) p;;
		let rec shift_down h i = 
			let l_c = 2*i+1 in if size h<=l_c then h else
				let b_c = (if size h=l_c+1 then l_c else max_index h l_c (l_c+1)) in 
					if max_index h i b_c = i then h else shift_down (swap h b_c i) b_c;;
		let delete_last h = Array.sub h 0 (size h - 1);;
		let delete h i = h.(i)<- h.(size h -1); shift_down (delete_last h) i;;
		let delete_max h = delete h 0;;
	end;;
	

let h = [|100;19;36;17;3;25;1;2;7|];;
MaxHeap.shift_up [|100;19;36;17;3;25;1;2;70|]	 8;;

MaxHeap.shift_down [|10;19;36;17;3;25;1;2;7|]	 0;;
module type MAXHEAP = 
  sig
    val size : 'a array -> int
    val find_max : 'a array -> 'a
    val delete : 'a array -> int -> 'a array
    val delete_max : 'a array -> 'a array
  end
