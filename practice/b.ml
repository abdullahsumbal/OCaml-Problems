
 type 'a node =
    | One of 'a 
    | Many of 'a node list


 let rec flat acc = function
 | [] -> acc
 | (One a)::d -> flat (a::acc) d
 | (Many a)::d -> flat (flat acc a) d
  	

let t_flat = flat [] [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]




let rec compress prev = function
| [] -> []
| h::d -> if prev = h then compress h d else h::(compress h d)

let list_compress' = compress "" ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;



let rec pack l current acc = match l with  
| [] -> acc
| [x] -> (x :: current) :: acc
| h1::h2::t -> if h1 = h2 then pack (h2::t) (h1::current) acc else pack t [] ((h1::current)::acc)
| h1::t-> acc





let res_pack = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] [] []



type ing = Choco | Nuts | Water | Dog
type cupcake = int * int * ing list

let c1 = (1,1,[Choco;Water;Dog])
let c2 = (2,3,[Dog;Nuts])

let cake_list = [c1;c2]

let all = [Dog;Water]

let all_free all' cake_list' = (List.filter (fun (a,b,c)-> if (List.exists (fun x -> List.exists (fun y-> y=x) c) all') then false else true) cake_list') 


let ans =  all_free all cake_list