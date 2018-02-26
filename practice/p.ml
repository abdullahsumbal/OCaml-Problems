(* Part 1: define a function that computes the parity of a list of
   booleans.

   What we want is a function even_parity such as even_parity l
   returns a boolean that combined with the elements of l the 
   number of 1’s (true) is even.
   This is a simple but important algorithm in data communication, its
   purpose is to detect errors (one bit flips). It is very easy to
   implement in circuit but it is not very reliable.
 *)

let rec even_parity = function
  | [] -> false
  | true::xs -> not (even_parity xs)
  | false::xs -> even_parity xs

(* This version is very natural but it is not tail recursive. Let's
   make the tail recursive version. *)

let even_parity_tr l =
  let rec parity p = function
    | [] -> p
    | p'::xs -> parity (p<>p') xs
  in
  parity false l

(* Part 2: Now prove that both functions are equivalent. You will need to use facts about the <> (XOR) operation *)

let ex_1 = [true]
let ex_2 = [true; true; false; false]
let ex_3 = [false; true; true; false; true]

let t_1 = even_parity_tr ex_1 
let t_2 = even_parity_tr ex_2 
let t_3 = even_parity_tr ex_3 
(* High order - croupier for simplified roulette *)

(* We have a simplified roulette, where we have only two colours that
   we can bet but if zero comes out, everyone loses *)

type colour = Red | Black        (* The two colours we can bet on *)

type result = colour option      (* The result of a run, it could be one of the colours or no colour if zero came up *)

type bet = int * colour          (* The bet amount and to what colour *)

type player = string * bet

let listOfbets: bet list= [(800,Black);(100,Red)]
(* It is simple to see who won *)
let compute (am, col : bet) : result -> int = function
  | None -> 0
  | Some col' -> if col = col' then am * 2 else 0

(*
Solve all these questions without using recursion or pattern
matching on lists, but instead just use the HO functions we saw
in class.
 *)

(* Q1:  given a list of bets compute the results *)
let compute_res (l: bet list) res = List.map (function (amt, col) -> ((compute(amt,col) res), col)) l

let ans_compute_res = compute_res listOfbets (Some Red)
(* Q2: given a list of bets and a result compute a list of winning bets *)

let winners (l : bet list) (res :result) = match res with
| None -> []
| Some col' -> List.filter (fun (amt, col) -> col = col') l

let betWinners = winners listOfbets (Some Black)
(* Q3: given a list of bets and a result compute how much money the casino needs to pay back *)

let rec casioPay l res = match l with 
| [] -> 0
| h::t -> compute h res + casioPay t res

let ans_casioPay = casioPay listOfbets (Some Red)

(* Q4: given a list of bets and a result compute if everyone won *)
let everyoneWon l res = List.for_all (fun x -> if (compute x res) > 0 then true else false) l

let ans_everyOneWon =  everyoneWon listOfbets (Some Red)
(* Q5: given a list of bets and a result compute if someone won *)

let someoneWon l res = List.exists (fun x -> if (compute x res) > 0 then true else false) l

let ans_someOneWon =  someoneWon listOfbets (Some Black)
(* Q6: given a list of bets return the highest winning *)
let rec highest_winning l res= 
	let rec highest l high = match l with 
		| [] -> high
		| h::t -> if (compute h res) > high then highest t (compute h res) else highest t high
	in highest l 0
	

let ans_highest = highest_winning listOfbets (Some Red)

(* Level-up (a bit more complicated) *)

(* Q7: given a list of bets and a result compute the balance for the casino, how much it made *)


(* Ninja level  *)

(* Q8: Can you sort the results by the amount they made? *)
type bst = Empty | Node of int * bst * bst
let empty = Empty

let is_empty = function
|Empty -> true
|Node (_,_,_) -> false

let rec insert n = function 
| Empty -> Node (n,Empty,Empty)
| Node(x,left,right) -> if x = n then Node (x,left,right)
				else if n < x then Node(x,(insert n left), right)
				else Node(x, left, (insert n right))

(* Implement the following functions
 val min : bst -> int
 val remove : int -> bst -> bst
 val fold : ('a -> int -> 'a) -> 'a -> bst -> 'a
 val size : bst -> int
 *)
let rec min =  function (* return smallest value in bst *)
| Empty -> 0
| Node(x , left,right) -> if (is_empty left) then min left else x

(* tree with n removed *)

let rec remove n = function
 Empty -> Empty
 | Node (m, left, right) ->
 if m = n then (
	 if (is_empty left) then right
	 else if (is_empty right) then left
	 else let x = min right in
	 Node(x, left, remove x right)
 )
 else if n < m then Node(m, (remove n left), right)
 else Node(m, left, (remove n right)) 

 let f (h::d) a b = if h+.a > 0.0 then b else b

let rec fold f a l = match l with
 [] -> a
 | (h::t) -> fold f (f a h) t


let remove_ass v lst =
 fold (fun a h -> a (*if h = (_,v) then a else (h::a)*)) [ ] lst

 
 let insert_at_n lst ele n = 
 	let rec insert_at_n' index lst = match lst with
 	| [] -> []
 	| h::[] -> h::ele::[]
 	| h::t -> if index = n then
 				ele::h::t
 			  else 
 			  	h::(insert_at_n' (index+1) t)
 	in insert_at_n' 0 lst


let rec find_kth k = function 
	| [] -> None
	| h :: t -> if k = 1 then Some h else find_kth (k-1) t

let kth = find_kth 2 [1;2;3;4;4]

let rec find_length =  function
| [] -> 0
| h::t -> 1+ find_length t

let length = find_length [1;2;3;4;4]

