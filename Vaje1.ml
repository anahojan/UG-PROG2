print_string "Hello World!\n";;

1;;

true;;

true || false;;

4
+
78;;

(+);;

(<);;

22<56;;

let a = 12;;

let b = true;;

let rec zdruzi list1 list2 = match (list1, list2) with
 | ([], []) -> []
 | (a, []) -> a
 | ([], b) -> b
 | (a::b, c::d) ->
 if (a < c ) then a::(zdruzi b (c::d))
 else if (c<a) then c::(zdruzi (a::b) d)
 else a::(zdruzi b d);;

();;

print_string;;

(* to je komentar

 se vedno komentar
 *)


true && false;;

not true;;

if true then false else true;;

if false then 2 else false;;


12 - -2

123.+.12.34

(+.);;

int_of_float 12.8
float_of_int 12

( ** );;

2.**10.;;

(* characters *)

'c';;
'd';;
int_of_char 'd ';;


(* nizi *)
"to je niz!"

string_of_int 2022;;

string_of_int (1+1);;

1<12;;

12.<12.;;

if (1>53) then (int_of_float(2.3**2.)) else (2+23)
