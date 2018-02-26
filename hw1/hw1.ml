(* Student information:

   Enter your name, and if you chose to work in pairs, the name of the
   student you worked with (both students MUST submit the solution to
   myCourses):

   Name:
   McGill ID:

   If you worked in pairs, the name of the other student.

   Name:
   McGill ID:


 *)

(* Notice: by submitting as part of team, you declare that you worked
   together on the solution. Submissions in pairs are allowed to
   foster team work, they have to be developed by both students *)

(* Homework 1 - Questions 2 and 3 *)

(* First, some utility functions and declarations that you can use. Be
   sure to check Ocaml's documentation to find more functions
   available to you.

   You can start checking the documentation at:
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
 *)

(* the value of pi *)
let pi : float =  acos ~-.1.0

(* a function to compare floats that allows for some imprecision *)
let cmp n m = abs_float (n -. m) < 0.0001

(* a simple test of positivity *)
let positive a = a > 0.0

(* a function to check if a is multiple of b *)
let is_multiple_of (a : float) (b : float) : bool =
  let m = a /. b in
  cmp (m -. floor m) 0.0

(* a function to check if a is between plus/minus b *)
let abs_btwn a b = a < b && a > ~-.b

(* Question 2: Triangles are the best *)

type side = float

type tr_by_sides = side * side * side

type tr_kind
  = Scalene
  | Equilateral
  | Isosceles

(* Question 2.1 *)
let well_formed_by_sides (a, b, c : tr_by_sides) : bool =
	if positive a && positive b && positive c then
    if a +. b > c && a +. c > b && b +. c > a then
		  true
	 else
		false
  else
	  false


(* Question 2.2 *)
let create_triangle (kind : tr_kind) (area : float) : tr_by_sides = match kind with
    Equilateral -> let side_length = sqrt(area *. 2.0 *. sqrt(4.0 /. 3.0)) in (side_length, side_length, side_length)
  | Isosceles ->  let base = 2.0 in
                  let height = area in
                  let two_sides_length = sqrt( 1.0 +. height *. height) in
                    (two_sides_length, two_sides_length, base)
  | Scalene ->  let side_1_length = area +. 1.0 in
                let side_2_length = 2.0 in
                let side_3_angle = asin((area *. 2.0) /. (side_1_length *. side_2_length)) in
                let side_3_length = sqrt((side_1_length *. side_1_length) +. (side_2_length *. side_2_length) -. 2.0 *. side_1_length *. side_2_length *. cos(side_3_angle)) in
                  (side_1_length, side_2_length, side_3_length)
(* Question 2.3 *)
type angle = float

type tr_by_angle = side * side * angle

let well_formed_by_angle (a, b, gamma) : bool =
  (positive a && positive b && positive gamma) &&
    (not (is_multiple_of gamma pi))

let sides_to_angle (a, b, c : tr_by_sides) : tr_by_angle option =
  if well_formed_by_sides(a, b, c) then
    let angle = acos((a ** 2.0 +. b ** 2.0 -. c ** 2.0) /. (2.0 *. a *. b)) in
      Some (a, b, angle)
  else
    None

let angle_to_sides (a, b, gamma) =
  let side_length = sqrt(a ** 2.0 +. b ** 2.0 -. 2.0 *. a *. b *. cos(gamma)) in
    (a, b, side_length)

(* Now that you implemented Q2.2 and saw the new representation of
   triangles by two sides and the angle between them, also ponder on
   how one represents Q2.2 using this representation. The idea is to
   think about how the representation helps make illegal states hard
   to represent and how easy and hard it is to implement the
   algorithm. *)

(* Question 3: Flexing recursion and lists *)

let even (n : int) : bool = n mod 2 = 0

(* Question 3.1 *)
let evens_first (l : int list) : int list =
      let rec create_list (l : int list) even_list  odd_list=
      match l with
      | [] -> even_list@odd_list
      | h::t -> if even h then
            (*let y = even_list@[h] in*)
            create_list(List.tl l) (even_list@[h]) odd_list
            else
            let y = odd_list@[h] in
            create_list(List.tl l) even_list y
      in

      create_list l [] []

let ex_1 = evens_first [7 ; 5 ; 2; 4; 6; 3; 4; 2; 1]
 (*val ex_1 : int list = [2; 4; 6; 4; 2; 7; 5; 3; 1] *)

(* Question 3.2 *)
let even_streak (l : int list) : int =
  let rec find_even_streak l highest_streak current_streak = match l with
  | [] -> highest_streak
  | h::t -> if even h then
              if current_streak + 1 > highest_streak then
                find_even_streak (List.tl l) (current_streak + 1) (current_streak + 1)
              else
                find_even_streak (List.tl l) highest_streak current_streak
            else
              find_even_streak (List.tl l) highest_streak 0
  in find_even_streak l 0 0


let ex_2 = even_streak [7; 2; 4; 6; 3; 4; 2; 1]

(* val ex_2 : int = 3 *)


(* Question 3.3 *)

type nucleobase = A | G | C | T

let compress (l : nucleobase list) : (int * nucleobase) list =
  let rec rec_compress l compressed_list current_nucleo nucleo_count= match l with
  | [] -> compressed_list@[(nucleo_count, current_nucleo)]
  | h::t -> if h = current_nucleo then
              rec_compress t compressed_list h (nucleo_count + 1)
            else
              let appened_compressed_list = compressed_list@[(nucleo_count, current_nucleo)] in
              rec_compress t appened_compressed_list h 1
  in rec_compress l [] (List.hd l) 0


let rec decompress (l : (int * nucleobase) list) : nucleobase list =
  let rec rec_decompress l decompressed_list current_count = match l with
    | [] -> List.rev decompressed_list
    | h::t -> if current_count = 0 then
                  if (List.length l) > 1 then
                    let (count, nucleo) = (List.nth l 1) in 
                    rec_decompress (List.tl l) decompressed_list count
                  else
                    rec_decompress (List.tl l) decompressed_list (current_count - 1)
              else
                let (new_count, nucleo) = h in
                let new_list = nucleo::decompressed_list in
                rec_decompress l new_list (current_count - 1)

  in let (count, nucleo) = (List.hd l) in rec_decompress l [] count
let sample_dna : nucleobase list = [A;A;T;A;G;G;A;A;G;G;T;T;C;T;C]

let ex_3 = compress sample_dna

let ex_4 = decompress ex_3

let res_3_4 = sample_dna = ex_4 (* This should be true if everything went well *)
