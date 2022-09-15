type barva = RGB | CMYK | BW;;

type barva = RGB of int*int*int | CMYK of int*int*int*int | BW of int ;;

BW 255

type binarno_drevo = 
	| Node0 of int
	| Node1 of int*binarno_drevo
	| Node2 of int*binarno_drevo*binarno_drevo;;

Node2 (1,Node2 (0,Node0 (-1), Node0 3), Node1 (4, Node0 19));;

let d1 = Node2 (3,Node2 (0,Node0 (-1), Node0 1), Node1 (4, Node0 19));;

let rec dodaj drevo s = match drevo with
| Node0 a 		 -> Node1 (a,Node0 s)
| Node1 (a,b)  -> Node2 (a,b,Node0 s)
| Node2 (a,b,c)-> Node2 (a,dodaj b s,c);;

dodaj d1 55;;

let rec vstavi drevo s = match drevo with
| Node0 a 		 -> Node1 (a,Node0 s)
| Node1 (a,b)  -> Node2 (a,b,Node0 s)
| Node2 (a,b,c)-> Node2 (a,vstavi b s,c);;

let rec dodaj drevo s = match drevo with
| Node0 a when s>a -> Node1 (s,Node0 a)
| Node0 a 				 -> Node1 (a,Node0 s)
| Node1 (a,b) when s>a -> Node2 (a,b,Node0 s)
| Node1 (a,b) 				 -> Node1 (a,dodaj b s)
| Node2 (a,b,c) when s>a -> Node2 (a,b,dodaj c s)
| Node2 (a,b,c) 			 	 -> Node2 (a,dodaj b s,c);;


let d1 = Node2 (3,Node2 (0,Node0 (-1), Node0 1), Node1 (19, Node0 4));;

dodaj d1 15

type 'a binarno_drevo = 
	| Node0 of 'a
	| Node1 of 'a * 'a binarno_drevo
	| Node2 of 'a * 'a binarno_drevo * 'a binarno_drevo;;

