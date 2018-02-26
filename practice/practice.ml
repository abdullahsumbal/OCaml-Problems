(* We define a type of simple binary trees with data in their nodes *)
type 'a tree
  = Leaf
  | Node of 'a tree * 'a * 'a tree

(* We can write a function that counts how many nodes satisfy the predicat p *)
let rec count p = function
  | Leaf -> 0
  | Node (l, x, r) ->
     let cnt = count p l + count p r in
     if p x then cnt + 1 else cnt

(* Note that the direct version of the function is not tail recursive.
   What would it take to make it recursive? Well, a simple accumulator
   cannot be enough because we need to get rid of two recursive calls.
   What we need is a success continuation that counts how many types p
   is satisfied *)

(* Let's implement count_k that is tail recursive. Pay special
   attention to the type annotation, if we respect this type
   the resulting function will be tail recursive! *)

let rec count_k (p : 'a -> bool) (t : 'a tree) (k : int -> 'b) : 'b =  
  match t with 
  | Leaf -> k 0
  | Node (l,x,r) -> 
     if p x  then 
       count_k p l (fun m -> count_k p r (fun n -> k (m + n + 1 )))
     else 
       count_k p l (fun m -> count_k p r (fun n -> k (m + n)))

(* Now you can write count' that behaves like count but it is tail
   recursive (of course, we will use count_k to do it) *)
let count' p t = assert false


(* The very general type affords us many benefits, tail recursion, and
   versatility). Consider we are not interested in how many results we
   have, but just if there are too many of them (we all now that more
   thatn 5 results is too many) *)


(* We can implement this function by taking advantage of the
   continuation. For the observant student, this function will be less
   efficient than what it technically could because it always counts
   all the results instead of stopping after 5 *)
let too_many p t = count_k p t (fun n -> n > 5)

(* The mapping function on lists can be also generalized to other data
   structures, like trees. In this example we write that function for
   trees. *)
let rec map_tree (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match t with
  | Leaf -> Leaf
  | Node (l, x, r) -> Node (map_tree f l, f x, map_tree f r)

(* As before, many functions on trees are difficult to write in a tail
   recursive way without continuations. However, continuations make it
   easy, let's write map with continuations. Again, consider the type
   of the continuation passing version. *)

let rec map_tree_k (f : 'a -> 'b) (t : 'a tree) (k : 'b tree -> 'c) : 'c = match t with
  | Leaf -> k Leaf
  | Node (l,x,r) -> map_tree_k f l (fun y -> map_tree_k f r (fun z -> k( Node ( y,f x,z ))))