(* Q1: A Rose by any Other Name Would Smell as Sweet *)

type 'a rose_tree = Node of 'a * ('a rose_tree) list

(* Find with exceptions *)

exception BackTrack

(* Q1.1 write a function that finds an element of the tree using backtracking with exceptions *)

(* Function with exceptions *)
let rec find_e (p : 'a -> bool) (t : 'a rose_tree) : 'a = 
  let Node (value, tList) = t in
  if p value then value
  else
    let rec find_e' tList' = match tList' with
      | [] -> raise BackTrack
      | h::d -> (try find_e p h with BackTrack -> find_e' d)
    in find_e' tList

(* Q1.1: write this function and it helper functions *)
(* call find_e and handle the exceptions *)
let find p t = (try find_e p t with BackTrack -> raise BackTrack) 

(* Find with failure continuations *)

let rec find_k (p : 'a -> bool) (t : 'a rose_tree) (k : unit -> 'a option) : 'a option =
  let Node (value, tList) = t in 
  if p value then Some value
  else
    let rec find_k' tList' k = match tList' with
      | [] -> k ()
      | h::d -> find_k p h (fun () -> find_k' d k)
    in find_k' tList k
(* Q1.2: write this function and it helper functions *)
(*  call find_k with the appropriate inital continuation *)
let find' p t = find_k p t (fun () -> None) 

(* Find all with continuations *)
let rec find_all_k  (p : 'a -> bool) (t : 'a rose_tree) (k : 'a list -> 'b) : 'b = 
  let Node (value, tList) = t in
  if p value then find_all_k' p tList (fun r -> k (value::r))
  else find_all_k' p tList k
and
  find_all_k' p tList k = match tList with
  | [] -> k []
  | h::d -> find_all_k p h (fun r -> (find_all_k' p d (fun x -> k (x@r))))



(* Q1.3: write this function and it helper functions *)
let find_all p t = find_all_k p t (fun r -> r)

(* An example to use *)

let example = Node (7, [ Node (1, [Node (18, [])])
                         ; Node (2, [Node (16, [])])
                         ; Node (4, [])
                         ; Node (9, [])
                         ; Node (11, [])
                         ; Node (15, [])
                         ])
let example1 = Node (7, [Node (1, [])])
let is_big x =  x > 10

let q1 = find (fun x -> x = 16) example
let q2 = find' (fun x -> x = 16) example
let q3 = find_all (fun x-> x > 0 ) example
(* Q2 : Rational Numbers Two Ways *)

type fraction = int * int

module type Arith =
  sig
    type t
    val epsilon : t             (* A suitable tiny value, like epsilon_float for floats *)

    val plus : t -> t -> t      (* Addition *)
    val from_fraction : fraction -> t
    val to_string : t -> string 
    val minus : t -> t -> t     (* Substraction *)
    val prod : t -> t -> t      (* Multiplication *)
    val div : t -> t -> t       (* Division *)
    val abs : t -> t            (* Absolute value *)
    val lt : t -> t -> bool     (* < *)
    val le : t -> t -> bool     (* <= *)
    val gt : t -> t -> bool     (* > *)
    val ge : t -> t -> bool     (*>=*)
    val eq : t -> t -> bool     (* = *)
    val from_fraction : fraction -> t (* conversion from a fraction type *)
    val to_string : t -> string        (* generate a string *)
  end

module FloatArith : Arith =
struct
  type t = float
  let epsilon = epsilon_float
  let from_fraction (num, den) = float_of_int num /. float_of_int den

  let plus = (+.)
  let minus = (-.)
  let prod = ( *. )
  let div = ( /. )
  let abs = abs_float
  let lt = (<)
  let le = (<=)
  let gt = (>)
  let ge = (>=)
  let eq = (=)
  let to_string x = string_of_float x
end

(* Q2.1: Implement the Arith module using rational numbers (t = fraction) *)

module FractionArith : Arith =
struct
  type t = fraction
  let epsilon = (1,1000000)
  let from_fraction frac = frac

  let plus fracA fracB = let (n1,d1),(n2,d2) = fracA,fracB in
  match (n1,d1),(n2,d2) with
  | (_,0),(_,_) -> (0,0)
  | (_,_),(_,0) -> (0,0)
  | (_,_),(_,_) -> ((n1 * d2 + n2 * d1),(d1 * d2))

  let minus fracA fracB = let (n1,d1),(n2,d2) = fracA,fracB in
  match (n1,d1),(n2,d2) with
  | (_,0),(_,_) -> (0,0)
  | (_,_),(_,0) -> (0,0)
  | (_,_),(_,_) -> ((n1 * d2 - n2 * d1),(d1 * d2))

  let prod fracA fracB = let (n1,d1),(n2,d2) = fracA,fracB in
  match (n1,d1),(n2,d2) with
  | (_,0),(_,_) -> (0,0)
  | (_,_),(_,0) -> (0,0)
  | (_,_),(_,_) -> ((n1 * n2),(d1 * d2))

  let div fracA fracB = let (n1,d1),(n2,d2) = fracA,fracB in
  match (n1,d1),(n2,d2) with
  | (_,0),(_,_) -> (0,0)
  | (_,_),(_,0) -> (0,0)
  | (_,_),(_,_) -> ((n1 * d2),(d1 * n2))

  let abs frac = let (n,d)= frac in
  match (n > 0, d > 0) with
  | (true,true) -> (n,d)
  | (true, false) -> (n, -1 * d)
  | (false, true) -> (n * -1, d)
  | (false, false) -> (n * -1, d * -1)

  let lt fracA fracB = let (n1,d1),(n2,d2) = fracA,fracB in
  match (n1,d1),(n2,d2) with
  | (_,0),(_,_) -> false
  | (_,_),(_,0) -> false
  | (_,_),(_,_) -> let (x,y) = (minus fracA fracB) in (x/y < 0)

  let le fracA fracB = let (n1,d1),(n2,d2) = fracA,fracB in
  match (n1,d1),(n2,d2) with
  | (_,0),(_,_) -> false
  | (_,_),(_,0) -> false
  | (_,_),(_,_) -> let (x,y) = (minus fracA fracB) in x/y <= 0

  let gt fracA fracB = let (n1,d1),(n2,d2) = fracA,fracB in
  match (n1,d1),(n2,d2) with
  | (_,0),(_,_) -> false
  | (_,_),(_,0) -> false
  | (_,_),(_,_) -> let (x,y) = (minus fracA fracB) in x/y > 0

  let ge fracA fracB = let (n1,d1),(n2,d2) = fracA,fracB in
  match (n1,d1),(n2,d2) with
  | (_,0),(_,_) -> false
  | (_,_),(_,0) -> false
  | (_,_),(_,_) -> let (x,y) = (minus fracA fracB) in x/y >= 0

  let eq fracA fracB = let (n1,d1),(n2,d2) = fracA,fracB in
  match (n1,d1),(n2,d2) with
  | (_,0),(_,_) -> false
  | (_,_),(_,0) -> false
  | (_,_),(_,_) -> let (x,y) = (minus fracA fracB) in x/y = 0

  let to_string frac : string = let (n,d) = frac in (string_of_int n) ^ "/" ^ (string_of_int d)
end

module type NewtonSolver =
  sig
    type t

    val square_root : t -> t
  end

(* Examples question2 *)
let frac1 = FractionArith.from_fraction (25,1000)
let frac2 = FractionArith.from_fraction (26,45)
let acc = FractionArith.from_fraction (-1,100)
let ex_minus = FractionArith.to_string (FractionArith.minus FractionArith.epsilon acc)
let ex_prod = FractionArith.to_string (FractionArith.prod frac1 frac2)
let ex_abs = FractionArith.to_string (FractionArith.abs (FractionArith.prod frac1 acc))
let ex_gt = (FractionArith.gt frac1 frac2)
let ex_plus = FractionArith.to_string (FractionArith.plus frac1 acc)

(* let z = FractionArith.prod y y
let z_string = FractionArith.to_string z
let greaterthan = FractionArith.gt FractionArith.epsilon acc

let xy = FractionArith.to_string (FractionArith.abs(FractionArith.minus x z)) *)



(* Q2.2: Implement a function that approximates the square root using  the Newton-Raphson method *)

module Newton (A : Arith) : (NewtonSolver with type t = A.t) =
  struct
    type t = A.t
    let square_root a = 
      let rec find_root x acc = 
      if A.gt A.epsilon acc then x
      else
        let int_2 = A.from_fraction (2,1) in
        let approx_root = A.div  (A.plus (A.div a x) x)  int_2  in
          find_root approx_root (A.abs (A.minus a (A.prod approx_root approx_root)))
      in find_root (A.from_fraction (1,1)) (a)
  end

(* Examples *)

module FloatNewton = Newton (FloatArith)
module RationalNewton = Newton (FractionArith)

let sqrt2 = FloatArith.to_string( FloatNewton.square_root (FloatArith.from_fraction (49, 1)))
let sqrt2_r = FractionArith.to_string (RationalNewton.square_root (FractionArith.from_fraction (49, 1)))

(* Q3 : Real Real Numbers, for Real! *)

type 'a stream = { head : 'a  ; tail : unit -> 'a stream}

let rec nth z = function
  | 0 -> z.head
  | n -> nth (z.tail()) (n - 1)

let rec constant x = {head = x ; tail = fun () -> constant x }

(* Some examples *)

let sqrt2 =
  {head = 1 ; tail = fun () -> constant 2} (* 1,2,2,2,2... *)

let golden_ratio = constant 1   (* 1,1,1,1,1,1 *)

let rec take n z =
  if n = 1 then [z.head]
  else z.head::(take (n-1) (z.tail()))

(* Q3.1: implement the function q as explained in the pdf *)
let rec q z n = assert false

(* Q3.2: implement the function r as in the notes *)
let rec r z n = assert false

(* Q3.3: implement the error function *)
let error z n = assert false

(* Q3.4: implement a function that computes a rational approximation of a real number *)
let rat_of_real z approx = assert false

let real_of_int n = { head = n ; tail = fun () -> constant 0}

(* Q3.5: implement a function that computes the real representation of a rational number   *)
let rec real_of_rat r = assert false


(* Examples *)

(* let sqrt_2_rat = rat_of_real sqrt2 1.e-5 *)
(* let golden_ratio_rat = rat_of_real golden_ratio 1.e-5 *)
