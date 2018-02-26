(*Return the last element of the list*)

let rec lastElementOfList l = match l with 
| [] -> None
| h1::h2::[] -> Some h2
|  h::t -> lastElementOfList t 

let prob1 = lastElementOfList [1;2;2;2;2;3];;

(*         #use "nintynineproblems.ml";;        *)

(* Find the last but one (last and penultimate) elements of a list.*)

let rec findlastTwo =function
| [] -> []
| h1::h2::[] -> [h1;h2]
| h::t -> findlastTwo t

let prob2 = findlastTwo [1;2;2;2;2;3];;

(*Find the k'th element of a list.*)

let rec find_kth k = function
| [] -> None
| h::t -> if k = 0 then Some h else find_kth (k-1) t


let prob3 = find_kth 3 [1;2;3;34;5;65]

(*. Find the number of elements of a list.*)

let length l = List.length l

let prob4 = length [1;3;3;3;3;3]

(*Reverse a list*)

let pro5 = List.rev [3;3;2;3;23;2;2]

(* Find out whether a list is a palindrome*)

let palindrome = ['m';'g';'m']
let prob6 = (palindrome = List.rev palindrome)


(*Flatten a nested list structure. (medium)*)
 type 'a node =
    | One of 'a 
    | Many of 'a node list



let rec flatten acc tr  = match tr with
| [] -> acc
| (One h)::t -> flatten (acc@[h]) t
| (Many h)::t -> flatten (flatten acc h) t

let prob7 = flatten [] [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]


(*Eliminate consecutive duplicates of list elements. (medium)*)

let compress_0 l = List.rev (List.fold_left (fun acc h-> if h = (List.hd acc) then acc else h::acc) [List.hd l] l)

let compress_1 l = List.fold_right (fun h acc-> if h = (List.hd acc) then acc else h::acc) l [List.hd (List.rev l)] 

let prob8_0 = compress_0 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
let prob8_1 = compress_1 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

(*         #use "nintynineproblems.ml";;        *)

(*Pack consecutive duplicates of list elements into sublists. (medium)*)

let  rec pack l acc = match l with
| [] -> []
| h1::h2::t-> if h1 = h2 then (pack (h2::t) (h1::acc)) else (h1::acc)::(pack (h2::t) [])
| h1::t -> (h1::acc)::(pack t [])


let prob9 = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] []

(*Run-length encoding of a list. (easy)*)

let encode l =
	let rec  encoding l count= match l with 
	| [] -> []
	| h1::h2::t -> if h1 = h2 then encoding (h2::t) (count + 1) else (count + 1, h1)::(encoding (h2::t) 0)
	| h1::t -> (count + 1, h1)::(encoding t 0)

in encoding l 0 

let prob10 =  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]

(*Modified run-length encoding. (easy)*)
type  'a node =
    | One of 'a
    | Many of int * 'a

let encode l =
	let rec  encoding l count= match l with 
	| [] -> []
	| h1::h2::t -> if h1 = h2 then encoding (h2::t) (count + 1) 
					else if count > 0 then  (Many (count + 1, h1))::(encoding (h2::t) 0)
					else (One h1)::(encoding (h2::t) 0)
	| h1::t -> 	if count > 1 then  (Many (count + 1, h1))::(encoding (t) 0)
				else (One h1)::(encoding (t) 0)

in encoding l 0 


let prob11 = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;


(*Decode a run-length encoded list. (medium)*)



let decode l =
	let rec decoding l count= match l with
	| [] -> []
	| (One  a)::t -> a::(decoding t 0)
	| (Many (c, v))::t -> if c > 0  then v::( decoding ((Many (c - 1, v))::t) (count + 1)  ) 
						 else decoding t 0
in decoding l 0

let prob12 = decode [Many (4, "a"); One "b"; One "c"; One "a"; One "d"; Many (4, "e")]

(*         #use "nintynineproblems.ml";;        *)

(*Duplicate the elements of a list. (easy)*)

let rec duplicate l = List.fold_right (fun h acc-> h::h::acc ) l []

let prob14 = duplicate [1;3;2;5;6;7;4;3;2;2]

(*Replicate the elements of a list a given number of times. (medium)*)

let replicate l n =
	let rec replicate' count l = match l with 
	| [] -> []
	| h::t -> if count = 0 then replicate' n t else h::(replicate' (count - 1) (h::t) )

	in replicate' n  l

let prob15 = replicate ["s";"g"] 3


(*Drop every N'th element from a list. (medium)*)

let rec drop l n = match l with 
| [] -> []
| h::t -> if n = 0 then t else h::(drop t (n - 1))

let prob16 = drop [1;2;4;5;6;7;8;9] 5

(*Split a list into two parts; the length of the first part is given. (easy)*)

let rec split l n acc = match l with 
| [] -> (acc, [])
| h::t -> if n= 0 then (acc, t) else split t (n-1) (h::acc)


let prob17 = split [1;2;2;2;2;2;2;2;4] 4 []

(*Extract a slice from a list. (medium)*)

let rec extract l start_i end_i = match l with
| [] -> []
| h::t -> if start_i < 1 && end_i = 0 then  []
		else if start_i < 1 && end_i > 0 then h::(extract t (start_i - 1) (end_i - 1) )
		else []@(extract t (start_i - 1) (end_i - 1) )


let prob18 = extract [0;1;2;3;4;5;6;7;8;9] 2 4
(*         #use "nintynineproblems.ml";;        *)

(*Rotate a list N places to the left. (medium)*)

let rec rotate l r acc =  false



let prob19 = rotate [1;2;3;4;6;7;8;9;2] 3 []

(*Remove the K'th element from a list. (easy)*)

let rec remove l k = match l with
| [] -> []
| h::t -> if k = 1 then t else h::(remove t  (k - 1) )

let prob20 = remove [1;2;23;4;4;5;6;7;8;9] 2



